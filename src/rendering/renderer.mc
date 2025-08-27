-- # Global Renderingipeline
--
-- This module defines the entry point for rendering an object tree into formatted output.
-- It traverses the parsed object structure, organizes its children,
-- reconstructs the source code, and writes the formatted documentation files to disk.
--
-- After the extraction phase (and the labeling phase), we obtain an `Object`.
-- From this object, generating documentation pages becomes straightforward.
--
-- Here’s how the renderer works:
--
-- - To produce the correct output format, we define a rendering interface in `renderer-interface.mc`,
--   which is implemented by each specific renderer. Then, we unify all language renderers inside the main `Renderer`,
--   allowing us to abstract away the output format and work uniformly.
--
-- - Rendering the children on an object’s page is relatively simple.
--   We can distinguish each child’s type via its `kind` field and display them in the desired order.
--   Additionally, linking to a child is easy thanks to its `namespace`, which provides a unique and structured identifier.
--
-- - Reconstructing the source code is by far the most challenging part.
--   We don’t just want to dump a raw string into the documentation,
--   we want syntax highlighting, block-by-block collapsibility (folding), and contextual formatting.
--   Importantly, this reconstruction must **re-use data from the children**:
--   otherwise, we would have to recompute syntax highlighting and toggle button placement from scratch,
--   which is clearly not an acceptable solution.
--
--   The toggling system is handled in `source-code-spliter.mc`.
--   The idea is to split the source code into three parts:
--     - `left` (or `preview`): the part shown on the left of the toggle button,
--     - `right` (or `hidden`): the collapsible part of the code,
--     - `trimmed`: separators or comments found at the end of the block.
--      Trimmed part should **not** be displayed in the current block’s source,
--      but instead passed to the parent, who may need them for its own reconstruction.
    
include "./preprocessor.mc"
include "./renderers/main-renderer.mc"
include "./source-code-reconstruction.mc"
include "./rendering-options.mc"
include "./files-opener.mc"
include "./name-context.mc"
include "./util.mc"

include "../extracting/objects.mc"

include "../global/util.mc"
include "../global/logger.mc"
include "../global/format.mc"

-- ## render
--
-- Entrypoint to rendering. This function traverses the entire `ObjectTree` and writes
-- structured documentation for each object node.
--
-- ### Behavior:
-- - Preprocesses the object tree
-- - Defines a recursive `render` function that:
--     - Filters children
--     - Recursively renders them
--     - Organizes them by type
--     - Writes formatted output to file
--     - Returns `RenderingData` for each node
let render : RenderingOptions -> ObjectTree -> () = use Renderer in
    lam opt. lam obj.

    preprocess obj opt;
    renderSetup opt;
    renderingLog "Beggining of rendering stage.";
    recursive
    let render: FileOpener ->  RenderingOptions -> ObjectTree -> (RenderingData, RenderingOptions) = lam opener. lam oldOpt. lam objTree.  -- # Global Renderingipeline
        let emptyPreview = lam obj.
            let trees = reconstructSourceCode (objSourceCode obj) [] in
            renderTreeSourceCode trees obj opt
        in

        objLog (objTreeObj objTree) oldOpt;

        switch objTree
        case ObjectNode { obj = { kind = ObjUse {}} & obj, sons = sons } then (emptyPreview obj, oldOpt)
        case ObjectNode { obj = { kind = ObjInclude {} } & obj, sons = [ p ] } then
            if and (objIsStdlib obj) oldOpt.noStdlib then (emptyPreview obj, oldOpt) else
            let res = render (fileOpenerCreate oldOpt.letDepth) oldOpt p in
            (emptyPreview obj, res.1)
        case ObjectNode { obj = { kind = ObjInclude {} } & obj, sons = [] } then (emptyPreview obj, oldOpt)
        case ObjectNode { obj = { kind = ObjInclude {} } & obj } then
             renderingWarn "Include with more than one son detected"; (emptyPreview obj, oldOpt)
        case ObjectNode { obj = obj, sons = sons } then

            match fileOpenerOpen opener obj oldOpt with Some { wc = wc, write = write, path = path, fileOpener = opener, displaySons = displaySons } then
                (match path with "" then () else renderingLog (concat "Rendering file " path));
                -- Push header of the output file
                write (renderHeader obj oldOpt);

                let recDatas: [[RenderingData]] = foldl (lam buffer. lam tree.
                    let obj = objTreeObj tree in
                    match obj.kind with ObjRecursiveBloc {} then
                        let datas = map (lam son. (render opener opt son).0) (objTreeSons tree) in
                        cons datas buffer
                    else buffer) [] sons
                in

                -- Recursive calls
                match foldl (lam arg. lam son.
                      let obj = objTreeObj son in
                      let opt = { arg.opt with nameContext = nameContextInsertIfHas arg.opt.nameContext obj (objGetPureLink obj arg.opt) } in
                      match render opener opt son with (son, opt) in
                      { opt = opt, sons = cons son arg.sons }
                      ) { sons = [], opt = oldOpt } sons with { sons = sons, opt = opt }
                in
                let sons = reverse sons in
                
                let sons = injectTests sons in
                 
                let trees = reconstructSourceCode (objSourceCode obj) sons in
                let data = renderTreeSourceCode trees obj opt in
    
                write (renderTopPageDoc data opt);

                -- Ordering objects in a set
                let set = buildSet sons recDatas in
    
                 -- Displays uses and includes
                let displayUseInclude = lam title. lam arr.
                    let title = match arr with [] then "" else match title with "" then "" else
                            renderSectionTitle title opt in
                    write title;
                    write (renderLinkList arr opt)
                in
    
                -- Displays types and con
                let displayDefault = lam title. lam arr. lam displaySons.
                    let title = match arr with [] then "" else match title with "" then "" else
                            renderSectionTitle title opt in
                    write title;
                    iter (lam u. write (renderDocBloc u displaySons opt)) arr
                in
    
                iter (lam a. displayUseInclude a.0 a.1) [("Using", set.sUse), ("Includes", set.sInclude), ("Stdlib Includes", if opt.noStdlib then [] else set.sLibInclude)];
                iter (lam a. displayDefault a.0 a.1 displaySons)
                    [("Types", set.sType), ("Constructors", set.sCon)];
                displayDefault "Languages" set.sLang true;
                iter (lam a. displayDefault a.0 a.1 displaySons)                 
                 [("Syntaxes", set.sSyn), ("Variables", set.sLet), ("Semantics", set.sSem), ("Mexpr", set.sMexpr)];

                -- Push the footer of the page
                write (renderFooter obj opt);

                (match wc with Some wc then fileWriteClose wc else ());
                (data, if objPreserveNameCtx obj then opt else oldOpt)
            else (emptyPreview obj, oldOpt)
        end
    in
    let res = render (fileOpenerCreate opt.letDepth) opt obj in ()
