-- # Global Rendering Pipeline
--
-- This module defines the entry point for rendering an object tree into formatted output.
-- It traverses the parsed object structure, organizes its children,
-- reconstructs the source code, and writes the formatted documentation files to disk.
--
-- After the extraction phase (and the labeling phase), we obtain an `Object`.
-- From this object, generating documentation pages becomes straightforward.
-- RenderingOptions contains very useful informations, such as nameContext. See rendering-options.mc
-- for more informations.
--
-- Here’s how the renderer works:
--
-- - To produce the correct output format, we define a rendering interface in `renderer-interface.mc`,
--   which is implemented by each specific renderer. Then, we unify all language renderers inside the main `Renderer`,
--   allowing us to abstract away the output format and work uniformly.
--
-- - Rendering the children on an object’s page is relatively simple.
--   We can distinguish each child’s type via its `form` field and display them in the desired order.
--   Additionally, linking to a child is easy thanks to its `namespace`, which provides a unique and structured identifier.
--
-- - Reconstructing the source code is by far the most challenging part.
--   We don’t just want to dump a raw string into the documentation;
--   we want syntax highlighting, block-by-block collapsibility (folding), and contextual formatting.
--   Importantly, this reconstruction must **re-use data from the children**:
--   otherwise, we would have to recompute syntax highlighting and toggle button placement from scratch,
--   which is clearly not an acceptable solution. For more information, see `source-code-spliter.mc`
--
-- NOTE: As we do not want a recursive block to be considered as regular children, we need to extract its children,
-- render them, and inject them into the node’s children list.
-- For example:
-- let x =
--   recursive
--   let y = 2
--   in 3
-- Here we want `y` to be considered a direct child of `x`, not the child of a recursive block. But we still need to compute
-- the RenderingData of the recursive block to be able to build the source code correctly. By rendering the recursive block we
-- lose information about children, as we do not keep grandchild information, so the only solution that preserves the architecture
-- is to unwrap all the recursive blocks and render them a second time. As Recursive is considered as a never object by the file-opener,
-- meaning all its children will not have documentation page, the writing part only occurs once.
            

include "./preprocessor.mc"
include "./renderers/main-renderer.mc"
include "./source-code-spliter.mc"
include "./rendering-options.mc"
include "./files-opener.mc"
include "./util.mc"

include "../extracting/objects.mc"

include "../global/util.mc"
include "../global/logger.mc"
include "../global/format.mc"

type RenderingResult = {
     renderedMap: HashMap String (),
     searchDatas: [SearchDictObj]
}

let render : use Objects in RenderingOptions -> Object -> RenderingResult = use Renderer in
    lam opt. lam obj.
    use Objects in
    
    let log = opt.log in
    
    let searchDatas = objToJsDict opt obj in
    
    preprocess obj opt;
    renderSetup opt;

    log "Beginning of rendering stage.";

    recursive
    let render: RenderedMap -> Object -> [RenderingData] -> { datas: RenderingData, renderedMap: RenderedMap } =
        lam renderedMap. lam obj. lam tests.

        let emptyPreview = lam obj.
            { datas = renderCreateRenderingData obj tests opt, renderedMap = renderedMap }
        in

        objLog obj opt;

        switch obj
        case ObjInclude { child = Some child } then
            let res = render renderedMap child [] in
            { emptyPreview obj with renderedMap = res.renderedMap }
        case ObjInclude { child = None {} } then emptyPreview obj
        case _ then

            let loc = objGetMyLocation obj opt in
            match renderedMapInsert renderedMap obj loc with
            { renderedMap = renderedMap, prune = prune } in
            
            if prune then
                { datas = renderCreateRenderingData obj tests opt, renderedMap = renderedMap }
            else

            match fileOpenerOpen obj opt with Some { wc = wc, write = write, path = path } then
                (match path with "" then () else log (concat "Rendering file " path));

                type Acc = { tests: [RenderingData], children: [RenderingData], renderedMap: RenderedMap } in
                let acc = foldl
                    (lam acc: Acc. lam child.
                        match child with ObjUtest {} then 
                            match render acc.renderedMap child [] with {renderedMap = renderedMap, datas = datas} in
                            {
                                children = cons datas acc.children,
                                tests = cons datas acc.tests,
                                renderedMap = renderedMap
                            }
                        else
                            match
                                if objHasTests obj then render acc.renderedMap child acc.tests
                                else render acc.renderedMap child []
                            with { datas = datas, renderedMap = renderedMap } in
                            { children = cons datas acc.children, tests = [], renderedMap = renderedMap }
                    ) { tests = [], children = [], renderedMap = renderedMap } (reverse (objChildren obj))
                in

                let renderedMap = acc.renderedMap in
                let children = acc.children in

                -- Build source code for the current node
                let data = renderCreateRenderingData obj tests opt in

                (if objRenderIt obj then                

                    write (renderHeader obj opt);
                    write (renderObjTitle 1 obj opt);
                    write (renderTopPageDoc data opt);

                    let children = removeDoubleNames children in

                    -- Order objects into a set
                    let set = buildSet children in

                     -- Display uses and includes
                    let displayIncludes = lam title. lam arr.
                        let title = match arr with [] then "" else match title with "" then "" else
                                renderSectionTitle title opt in
                        write title;
                        write (renderLinkList arr opt)
                    in

                    -- Display types and constructors
                    let displayDefault = lam title. lam arr.
                        let title = match arr with [] then "" else match title with "" then "" else
                                renderSectionTitle title opt in
                        write title;
                        iter (lam u. write (renderDocBloc u opt)) arr
                    in

                    iter (lam a. displayIncludes a.0 a.1)
                         [("Includes", set.sInclude),
                         ("Stdlib Includes", set.sLibInclude)];
                    iter (lam a. displayDefault a.0 a.1)
                        [("Types", set.sType),
                        ("Constructors", set.sCon),
                        ("Languages", set.sLang),
                        ("Syntaxes", set.sSyn),
                        ("Variables", set.sLet),
                        ("Semantics", set.sSem)];

                    -- Push the footer of the page
                    write (renderFooter obj opt);

                    (match wc with Some wc then fileWriteClose wc else ())
                else ());

                { datas = data, renderedMap = renderedMap }
            else emptyPreview obj
        end
    in

    match render opt.renderedMap obj [] with { renderedMap = renderedMap } in
    
    {
         renderedMap = renderedMap,
         searchDatas = searchDatas
    }
