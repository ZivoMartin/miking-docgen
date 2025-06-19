-- ## Rendering Module
-- This module provides the core logic to render Markdown documentation from a list of `DocTree` nodes.
-- It extracts structured comments and produces a `.md` file with the documentation, 
-- following an ordered display of definitions (use/include, types, constructors, language definitions, etc.).
--
-- Expects that a function capable of compiling a `DocTree` into an `MdObject` is provided.
-- The main entry point is the `render` function.


include "md-object.mc"


-- ## Compiler Type
--
-- A `Compiler` is a function capable of transforming a `DocTree` into an `MdObject`.
-- It takes as arguments:
-- - a `DocTree` to compile
-- - a `String` representing the current namespace path
-- - a list of `String` representing the current comment buffer
--
-- It returns:
-- - an updated comment buffer
-- - an `MdObject` representing the compiled element
--
-- See implementation in `compiler/compile.mc`
type Compiler = use MdObject in (DocTree -> String -> [String] -> ([String], Object))
    
-- ## Render Function
--
-- The main function that generates a Markdown file from a list of `DocTree` nodes.
-- 
-- ### Arguments:
-- - `compile`: the `Compiler` function to compile individual `DocTree` nodes
-- - `namespace`: the current namespace
-- - `currentComment`: current buffer of comments
-- - `mdTitle`: the title of the Markdown output
-- - `mdFilePath`: the output path to the Markdown file (relative)
-- - `sons`: the list of child `DocTree` nodes to process
--
-- ### Behavior:
-- - Opens the Markdown output file (under `doc-gen-output/` directory)
-- - Writes the global title and initial comments
-- - Recursively compiles each `DocTree` node and collects their `MdObject` representations
-- - Sorts and displays the collected objects in the following order:
--   - `use` / `include`
--   - `type`
--   - `con`
--   - `lang`
--   - `syn`
--   - `let` / `sem`
--   - `mexpr`
-- - Returns the updated comment buffer (remaining comments after the last block)
let render : (Compiler -> String -> [String] -> String -> String -> [DocTree] -> [String]) =
    use MdObject in
    lam compile. lam namespace. lam currentComment. lam mdTitle. lam mdFilePath. lam sons.
    -- Opening md file
    match fileWriteOpen (concat "doc-gen-output/" mdFilePath) with Some wc then
        let write = fileWriteString wc in


        let pushDoc = lam doc. iter (lam c. write (concat c "\n")) doc in
    
        -- Pushing title and global documentation
        write (concatAll ["# ", mdTitle, "\n\n"]);
        pushDoc currentComment;
        write "\n";
    
        -- Collecting objects
        let objects = foldl
            (lam arg. lam s.
            let obj = compile s namespace arg.0 in
            (obj.0, cons obj.1 arg.1))
            (currentComment, [])
            sons in

        -- Ordering objects in a set
        let set = 
            recursive
            let buildSet = lam set. lam objects. 
                switch objects
                case [obj] ++ objects then buildSet (switch obj
                    case MdUse {} then { set with mdUse = cons obj set.mdUse }
                    case MdLet {} then { set with mdLet = cons obj set.mdLet }
                    case MdLang {} then { set with mdLang = cons obj set.mdLang }
                    case MdSem {} then { set with mdSem = cons obj set.mdSem }
                    case MdSyn {} then { set with mdSyn = cons obj set.mdSyn }
                    case MdCon {} then { set with mdCon = cons obj set.mdCon }    
                    case MdMexpr {} then { set with mdMexpr = cons obj set.mdMexpr }
                    case MdType {} then { set with mdType = cons obj set.mdType }    
                    case MdInclude {} then { set with mdInclude = cons obj set.mdInclude }
                    case MdWord {} then set
                    end) objects
                case [] then set
                end
            in buildSet { mdUse = [], mdLet = [], mdLang = [], mdType = [], mdSem = [], mdSyn = [], mdCon = [], mdMexpr = [], mdInclude = [], mdType = [] } objects.1 in

        let pushLink = lam link. write (concatAll ["\n[-](", link,")\n\n"]) in
    
        -- Displays uses and includes
        let displayUseInclude = lam title. lam arr.
            let title = match arr with [] then "" else concatAll ["**", title, ":** \n\n"] in
            write title;
            let doc = foldl (lam arg. lam u.
                                match u with MdUse { l = l }  then
                                    cons (concat ", " l) arg
                                else match u with MdInclude { filePath = filePath } then
                                    cons (concat ", " filePath) arg
                                else never)
                            [] arr in
            let doc = switch concatAll (reverse doc)
            case ", " ++ doc then doc
            case doc then doc end in
         write (concat doc "\n\n")
        in

         -- Displays types and con
        let displayTypeCon = lam title. lam arr.
            let title = match arr with [] then "" else concatAll ["**", title, ":** \n"] in
            write title;
            iter (lam u.
                    match u with MdType { t = t, name = name, link = link }  then
                        let t = match t with Some t then concat " : " t else "" in
                        write (concatAll ["```ocaml\n type ", name, t, "\n```\n\n"]);
                        pushLink link
                    else match u with MdCon { t = t, name = name, link = link }  then
                        write (concatAll ["```ocaml\n con ", name, " : ", t, "\n```\n\n"]);
                        pushLink link
                    else never) arr
        in

        -- Displays lang definitions            
        let displayLang = lam f.
            write (match set.mdLang with [] then "" else "**Langs**:\n"); 
            iter (lam u.
                match u with MdLang { name = name, parents = parents, link = link }  then
                    let p = map (lam p. concat " + " p) parents in
                    let parents = switch concatAll p
                                case " + " ++ p then concat " : " p
                                case _ then "" end in
                    write (concatAll ["```ocaml\n lang ", name, parents, "\n```\n\n"]);
                    pushLink link
                else never) (reverse set.mdLang) in
    
         -- Displays syntax definitions            
        let displaySyn = lam f.
            write (match set.mdSyn with [] then "" else "**Syntaxes**:\n"); 
            iter (lam u.
                match u with MdSyn { name = name, link = link }  then
                    write (concatAll ["```ocaml\nsyn ", name, "\n```\n\n"]);
                    pushLink link
                else never) (reverse set.mdSyn) in


         -- Displays let and sem
        let displayLetSem = lam title. lam arr.
            let title = match arr with [] then "" else concatAll ["**", title, ":** \n"] in
            write title;
            iter (lam u.
                    match u with MdLet { name = name, link = link }  then    
                        write (concatAll ["```ocaml\n let ", name, "\n```\n"]);
                        pushLink link
                    else match u with MdSem { name = name, link = link }  then    
                        write (concatAll ["```ocaml\n sem ", name, "\n```\n"]);
                        pushLink link
                    else never) arr
        in

        let displayMexpr = lam f.
            match set.mdMexpr with [] then ()
            else match set.mdMexpr with [ MdMexpr { link = link } ] then
                write "```ocaml\n mexpr\n```\n";
                pushLink link
            else printLn "WARNING: 2 mexpr has been detected, not supported." in
            
        displayUseInclude "Using" set.mdUse;
        displayUseInclude "Include" set.mdInclude;
        displayTypeCon "Types" set.mdType;
        displayTypeCon "Constructors" set.mdCon;
        displayLang ();
        displaySyn ();
        displayLetSem "Variables" set.mdLet;
        displayLetSem "Sementics" set.mdSem;
        displayMexpr ();    
            
        fileWriteClose wc;
        objects.0
    else error "Error writing to file."
