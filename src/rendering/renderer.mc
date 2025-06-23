-- ## Rendering Module
-- This module provides the core logic to render Markdown documentation from an `ObjectTree`.
-- It goes through each nodes and generates all the pages
-- following an ordered display of definitions (use/include, types, constructors, language definitions, etc.).

include "preprocessor.mc"
include "../extracting/objects.mc"
include "util.mc"
include "../util.mc"    

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
let render : ObjectTree -> () = use ObjectKinds in lam obj.
    preprocess obj;
    recursive
    let render : ObjectTree -> () = lam obj.
        switch obj
        case ObjectNode { obj = { kind = ObjUse {}}, sons = sons } then ()
        case ObjectNode { obj = { kind = ObjInclude {} }, sons = [ p ] } then render p
        case ObjectNode { obj = { kind = ObjInclude {} }, sons = [] } then ()
        case ObjectNode { obj = obj, sons = sons } then
            -- Opening md file
            match createAndOpen (concat "doc-gen-output/" (objLink obj))  with Some wc then
                let write = fileWriteString wc in

                -- Pushing title and global documentation
                write (concatAll ["# ", objTitle obj, "\n\n"]);
                write (objGetSpecificDoc obj);
                write "\n\n";    
                write obj.doc;
                write "\n\n";

                -- Removing words
                let sons = filter (lam s. match s with ObjectNode {} then true else false) sons in
            
                -- Recursive calls
                iter render sons;

                -- Extracting infos
                let sons = foldl (lam a. lam s.
                    match s with ObjectNode { obj = obj } then
                        cons obj a
                    else
                        warn "sons should only contain ObjectNode at this stage.";
                        a  
                    ) [] sons in

                -- Ordering objects in a set
                let set = 
                    recursive
                    let buildSet = lam set. lam objects. 
                        switch objects
                        case [obj] ++ objects then buildSet (switch obj.kind
                            case ObjUse {} then { set with mdUse = cons obj set.mdUse }
                            case ObjLet {} then { set with mdLet = cons obj set.mdLet }
                            case ObjLang {} then { set with mdLang = cons obj set.mdLang }
                            case ObjSem {} then { set with mdSem = cons obj set.mdSem }
                            case ObjSyn {} then { set with mdSyn = cons obj set.mdSyn }
                            case ObjCon {} then { set with mdCon = cons obj set.mdCon }    
                            case ObjMexpr {} then { set with mdMexpr = cons obj set.mdMexpr }
                            case ObjType {} then { set with mdType = cons obj set.mdType }    
                            case ObjInclude { isStdlib = true } then { set with mdLibInclude = cons obj set.mdLibInclude }
                            case ObjInclude { isStdlib = false } then { set with mdInclude = cons obj set.mdInclude }    
                            end) objects
                        case [] then set
                        end
                    in buildSet { mdUse = [], mdLet = [], mdLang = [], mdType = [], mdSem = [], mdSyn = [], mdCon = [], mdMexpr = [], mdInclude = [], mdLibInclude = [], mdType = [] } sons in

                let pushLink = lam obj. write (concatAll ["\n[-](/", objLink obj,")\n\n"]) in

                -- Displays uses and includes
                let displayUseInclude = lam title. lam arr.
                    let title = match arr with [] then "" else concatAll ["**", title, ":** \n\n"] in
                    write title;
                    let doc = map (lam u. concatAll ["[", objTitle u, "](/", objLink u, ")"]) arr in
                    write (strJoin ", " (reverse doc));
                    write "\n\n"
                in

                 -- Displays types and con
                let displayDefault = lam title. lam arr.
                    let title = match arr with [] then "" else match title with "" then "" else
                            concatAll ["**", title, ":** \n"] in
                    write title;
                    iter (lam u. write (objMdFormat u); pushLink u) arr
                in
                    
                iter (lam a. displayUseInclude a.0 a.1) [("Using", set.mdUse), ("Includes", set.mdInclude), ("Stdlib Includes", set.mdLibInclude)];
                iter (lam a. displayDefault a.0 a.1)
                [("Types", set.mdType), ("Constructors", set.mdCon), ("Languages", set.mdLang),
                ("Syntaxes", set.mdSyn), ("Variables", set.mdLet), ("Sementics", set.mdSem),("", set.mdMexpr)];
                    
                fileWriteClose wc
                
            else ()
        case _ then () end
        
    in render obj
