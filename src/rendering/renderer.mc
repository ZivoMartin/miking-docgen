-- ## Rendering Module
-- This module provides the core logic to render Markdown documentation from an `ObjectTree`.
-- It goes through each nodes and generates all the pages
-- following an ordered display of definitions (use/include, types, constructors, language definitions, etc.).

include "preprocessor.mc"
include "../extracting/objects.mc"
include "util.mc"
include "../util.mc"    
include "md-renderer.mc"
include "html-renderer.mc"

lang Renderer = MarkdownRenderer + HtmlRenderer

    -- Returns the default format if nothing is specified in the CLI
    sem defaultFormat /- () -> Format -/ =
        | _ -> Html {}

end
    
-- ## Render Function
--
-- The main function that generates a Markdown file from an `ObjectTree`.
-- Takes in input the objects and returns unit.  
--
-- ### Behavior:
-- - Opens the Markdown output file (under `doc-gen-output/` directory)
-- - Writes the global title and top documentation
-- - Recursively go through each son to generate all the pages
-- - Sorts and displays the sub-objects in the following order:
--   - `use` / `include`
--   - `type`
--   - `con`
--   - `lang`
--   - `syn`
--   - `let` / `sem`
--   - `mexpr`
let render = use ObjectKinds in use Renderer in lam fmt. lam obj.
    preprocess obj;
    recursive
    let render : Format -> ObjectTree -> () = lam fmt. lam obj.
        switch obj
        case ObjectNode { obj = { kind = ObjUse {}}, sons = sons } then ()
        case ObjectNode { obj = { kind = ObjUtest {}}, sons = sons } then ()    
        case ObjectNode { obj = { kind = ObjInclude {} }, sons = [ p ] } then render fmt p
        case ObjectNode { obj = { kind = ObjInclude {} }, sons = [] } then ()
        case ObjectNode { obj = obj, sons = sons } then
            -- Opening a file
            let path = concat "doc-gen-output/" (objLink obj) in
            match createAndOpen path  with Some wc then
                let write = fileWriteString wc in

                -- Push header of the output file
                write (objFormatHeader (fmt, obj));

                -- Pushing title and global documentation
                write (objFormatedTitle (fmt, obj));
                write (objGetSpecificDoc (fmt, obj, sons));

                -- Removing words
                let sons = objectSonsFilterNodes sons in
            
                -- Recursive calls
                iter (render fmt) sons;

                -- Extracting infos
                let sons = foldl (lam a. lam s.
                    match s with ObjectNode { obj = obj, sons = sons } then
                        cons { obj = obj, sons = sons } a
                    else
                        warn "sons should only contain ObjectNode at this stage.";
                        a  
                    ) [] sons in

                -- Ordering objects in a set
                let set = 
                    recursive
                    let buildSet = lam set. lam objects. 
                        switch objects
                        case [obj] ++ objects then buildSet (switch obj.obj.kind
                            case ObjUse {} then { set with Use = cons obj.obj set.Use }
                            case ObjLet {} then { set with Let = cons obj set.Let }
                            case ObjLang {} then { set with Lang = cons obj set.Lang }
                            case ObjSem {} then { set with Sem = cons obj set.Sem }
                            case ObjSyn {} then { set with Syn = cons obj set.Syn }
                            case ObjCon {} then { set with Con = cons obj set.Con }    
                            case ObjMexpr {} then { set with Mexpr = cons obj set.Mexpr }
                            case ObjType {} then { set with Type = cons obj set.Type }    
                            case ObjInclude { isStdlib = true } then { set with LibInclude = cons obj.obj set.LibInclude }
                            case ObjInclude { isStdlib = false } then { set with Include = cons obj.obj set.Include }
                            case ObjUtest {} then set    
                            end) objects
                        case [] then set
                        end
                    in buildSet { Use = [], Let = [], Lang = [], Type = [], Sem = [], Syn = [], Con = [], Mexpr = [], Include = [], LibInclude = [], Type = [] } sons in

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
                    iter (lam u. write (objFormat (fmt, u.obj, u.sons))) (reverse arr)
                in

                iter (lam a. displayUseInclude a.0 a.1) [("Using", set.Use), ("Includes", set.Include), ("Stdlib Includes", set.LibInclude)];
                iter (lam a. displayDefault a.0 a.1)
                [("Types", set.Type), ("Constructors", set.Con), ("Languages", set.Lang),
                ("Syntaxes", set.Syn), ("Variables", set.Let), ("Sementics", set.Sem),("Mexpr", set.Mexpr)];

                -- Push the footer of the page
                write (objFormatFooter (fmt, obj));
                fileWriteClose wc
                
            else warn (concat "Failed to open " path)
        case _ then () end
        
    in render fmt obj
