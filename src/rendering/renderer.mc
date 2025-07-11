-- # Global Rendering Pipeline
--
-- This module defines the entry point for rendering an object tree into formatted output.
-- It traverses the parsed object structure, organizes its children,
-- reconstructs source code, and writes formatted files to disk.

include "preprocessor.mc"
include "../extracting/objects.mc"
include "../util.mc"    
include "md-renderer.mc"
include "./html-rendering/renderer.mc"
include "./source-code-reconstruction.mc"
include "../logger.mc"

-- Combines the Markdown and HTML renderers via language composition.    
lang Renderer = MarkdownRenderer + HtmlRenderer end


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
let render = use ObjectKinds in use Renderer in lam fmt. lam obj.
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
                write (objFormatHeader (fmt, obj));

                -- Pushing title and global documentation
                write (objFormatedTitle (fmt, obj));

                -- Removing words
                let sons = objectSonsFilterNodes sons in
            
                -- Recursive calls
                let sons: [RenderingData] = map (render fmt) sons in

                -- Pushing object documentation using data of the sons to reconstruct the source code
                let data = getRenderingData obj obj.sourceCode sons (getWordRenderer fmt) (getCodeHider fmt) in
                
                write (objGetSpecificDoc (fmt, data));

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
                            end) sons
                        case [] then set
                        end
                    in buildSet { Use = [], Let = [], Lang = [], Type = [], Sem = [], Syn = [], Con = [], Mexpr = [], Include = [], LibInclude = [], Type = [], Utest = [] } sons in

                -- Displays uses and includes
                let displayUseInclude = lam title. lam arr.
                    let title = match arr with [] then "" else match title with "" then "" else
                            getFormatedSectionTitle (fmt, title) in
                    write title;
                    write (getFormatedLinkList (fmt, arr))
                in

                 -- Displays types and con
                let displayDefault = lam title. lam arr.
                    let title = match arr with [] then "" else match title with "" then "" else
                            getFormatedSectionTitle (fmt, title) in
                    write title;
                    iter (lam u. write (objFormat (fmt, u))) (reverse arr)
                in

                iter (lam a. displayUseInclude a.0 (reverse a.1)) [("Using", set.Use), ("Includes", set.Include), ("Stdlib Includes", set.LibInclude)];
                iter (lam a. displayDefault a.0 (reverse a.1))
                [("Types", set.Type), ("Constructors", set.Con), ("Languages", set.Lang),
                ("Syntaxes", set.Syn), ("Variables", set.Let), ("Sementics", set.Sem), ("Mexpr", set.Mexpr), ("Tests", set.Utest)];

                -- Push the footer of the page
                write (objFormatFooter (fmt, obj));
                fileWriteClose wc;
                data
            else
                renderingWarn (concat "Failed to open " path);
                emptyPreview obj
        case ObjectLeaf _ then error "You should never try to render an ObjectLeaf." end
        
    in let res = render fmt obj in ()
