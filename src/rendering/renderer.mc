-- # Global Rendering Pipeline
--
-- This module defines the entry point for rendering an object tree into formatted output.
-- It traverses the parsed object structure, organizes its children,
-- reconstructs the source code, and writes the formatted documentation files to disk.
--
-- After the extraction phase (and optionally the labeling phase), we obtain an `Object`.
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
--   The idea is that each renderer defines a `WordRender`:
--   a function that takes a word (token) and returns, for example, a colored `<span>` for HTML.
--   During reconstruction, each word is passed through this function.
--
--   The toggling system is handled in `source-code-spliter.mc`.
--   The idea is to split the source code into three parts:
--     - `left` (or `preview`): the part shown on the left of the toggle button,
--     - `right` (or `hidden`): the collapsible part of the code,
--     - `trimmed`: separators or comments found at the end of the block.
--      Trimmed part should **not** be displayed in the current block’s source,
--      but instead passed to the parent, who may need them for its own reconstruction.
    
include "preprocessor.mc"
include "./renderers/main-renderer.mc"
include "./source-code-reconstruction.mc"
include "../logger.mc"
include "../format.mc"
include "../extracting/objects.mc"
include "../util.mc"    

-- ## render
--
-- Entrypoint to rendering. This function traverses the entire `ObjectTree` and writes
-- structured documentation for each object node.
--
-- ### Parameters:
-- - `fmt`: The rendering format (`Html`, `Markdown`, etc.)
-- - `obj`: The root `ObjectTree` to render
--
-- ### Behavior:
-- - Preprocesses the object tree
-- - Defines a recursive `render` function that:
--     - Filters children
--     - Recursively renders them
--     - Organizes them by type
--     - Writes formatted output to file
--     - Returns `RenderingData` for each node
let render : use Formats in Format -> ObjectTree -> () = use Renderer in
    lam fmt. lam obj.
    preprocess obj;
    renderingLog "Beggining of rendering stage.";
    recursive
    let render : Format -> ObjectTree -> RenderingData = lam fmt. lam objTree.
        let emptyPreview = lam obj. { left = [], right = [], trimmed = [], obj = obj } in            
        switch objTree
        case ObjectNode { obj = { kind = ObjUse {}} & obj, sons = sons } then emptyPreview obj
        case ObjectNode { obj = { kind = ObjInclude {} } & obj, sons = [ p ] } then let res = render fmt p in emptyPreview obj
        case ObjectNode { obj = { kind = ObjInclude {} } & obj, sons = [] } then emptyPreview obj
        case ObjectNode { obj = { kind = ObjInclude {} } & obj } then renderingWarn "Include with more than one son detected"; emptyPreview obj
        case ObjectNode { obj = obj, sons = sons } then
            -- Opening a file
            let path = concat "doc-gen-output/" (objLink obj) in
            renderingLog (concat "Rendering file " path);
   
            match fileWriteOpen path with Some wc then
                let write = fileWriteString wc in

                -- Push header of the output file
                write (renderHeader obj fmt);
                -- Pushing title and global documentation
                write (renderObjTitle 1 obj fmt);
                
                -- Extract all the `ObjectNode`s in an array of ObjectTree
                let unwrapRecursives : [ObjectTree] -> [ObjectTree] = use ObjectKinds in lam sons.
                    foldl (lam sons. lam son.
                        switch son
                        case ObjectNode { obj = { kind = ObjRecursiveBlock {} }, sons = blockSons } then
                            let blockSons = foldl (lam acc. lam son.
                                switch son
                                case ObjectNode { obj = { kind = ObjLet {} } } then cons son acc
                                case ObjectNode {} then renderingWarn "We should only have let node at this stage."; cons son acc
                                case _ then acc
                                end) [] blockSons in
                            concat blockSons sons
                        case ObjectNode {} then cons son sons
                        case _ then sons
                        end) [] (reverse sons) in
    
                -- Removing words
                let sons = unwrapRecursives sons in
            
                -- Recursive calls
                let sons: [RenderingData] = map (render fmt) sons in
                let trees = reconstructSourceCode (objSourceCode obj) sons in
                let data = renderTreeSourceCode trees obj fmt in
    
                write (renderSpecificDoc data fmt);

                -- Ordering objects in a set
                let set = 
                    recursive
                    let buildSet = lam set. lam sons. 
                        switch sons
                        case [son] ++ sons then buildSet (
                        switch son.obj.kind
                            case ObjUse {} then { set with Use = cons son.obj set.Use }
                            case ObjLet {} then { set with Let = cons son set.Let }
                            case ObjLang {} then { set with Lang = cons son set.Lang }
                            case ObjSem {} then { set with Sem = cons son set.Sem }
                            case ObjSyn {} then { set with Syn = cons son set.Syn }
                            case ObjCon {} then { set with Con = cons son set.Con }    
                            case ObjMexpr {} then { set with Mexpr = cons son set.Mexpr }
                            case ObjType {} then { set with Type = cons son set.Type }
                            case ObjUtest {} then { set with Utest = cons son set.Utest }
                            case ObjInclude { isStdlib = true } then { set with LibInclude = cons son.obj set.LibInclude }
                            case ObjInclude { isStdlib = false } then { set with Include = cons son.obj set.Include }
                            case ObjRecursiveBlock {} then renderingWarn "We should not get to RecursiveBlock at this stage."; set
                            end) sons
                        case [] then set
                        end
                    in buildSet { Use = [], Let = [], Lang = [], Type = [], Sem = [], Syn = [], Con = [], Mexpr = [], Include = [], LibInclude = [], Type = [], Utest = [] } (reverse sons) in
    
                 -- Displays uses and includes
                let displayUseInclude = lam title. lam arr.
                    let title = match arr with [] then "" else match title with "" then "" else
                            renderSectionTitle title fmt in
                    write title;
                    write (renderLinkList arr fmt)
                in
    
                -- Displays types and con
                let displayDefault = lam title. lam arr.
                    let title = match arr with [] then "" else match title with "" then "" else
                            renderSectionTitle title fmt in
                    write title;
                    iter (lam u. write (renderRenderingData u fmt)) arr
                in
    
                iter (lam a. displayUseInclude a.0 a.1) [("Using", set.Use), ("Includes", set.Include), ("Stdlib Includes", set.LibInclude)];
                iter (lam a. displayDefault a.0 a.1)
    
                [("Types", set.Type), ("Constructors", set.Con), ("Languages", set.Lang),
                ("Syntaxes", set.Syn), ("Variables", set.Let), ("Sementics", set.Sem), ("Mexpr", set.Mexpr), ("Tests", set.Utest)];
    
                -- Push the footer of the page
                write (renderFooter obj fmt);
                fileWriteClose wc;
                data
            else
                renderingWarn (concat "Failed to open " path);
                emptyPreview obj
        end
    in
    let res = render opt.fmt obj in ()




