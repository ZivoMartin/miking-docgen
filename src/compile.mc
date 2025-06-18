-- This file implement the compiler
-- Takes in input a doctree and generate all the md files

include "parser.mc"
include "doc-tree.mc"
include "ext/file-ext.mc"
include "fileutils.mc"

lang MdObject

syn Object = 
    | MdProgram { filePath: String, doc: [String] }
    | MdLet { name : String, doc: [String] }
    | MdLang { name : String, doc: [String] }
    | MdType { name: String, t: String, doc: [String] }
    | MdUse { l: String }
    | MdSem {}
    | MdSyn {}
    | MdCon {}
    | MdMexpr {}
    | MdWord { word: String }
    | MdInclude { filePath: String }

    sem getMdDoc = 
    | MdLet { name = name, doc = doc} -> concatAll ["let ", name]
    | MdLang {} -> error "TODO: MdLang getMdDoc"
    | MdType {} -> error "TODO: MdType getMdDoc"
    | MdUse {} -> error "TODO: MdUse getMdDoc"
    | MdSem {} -> error "TODO: MdSem getMdDoc"
    | MdSyn {} -> error "TODO: MdSyn getMdDoc"
    | MdCon {} -> error "TODO: MdCon getMdDoc"
    | MdMexpr {} -> error "TODO: MdMexpr getMdDoc"
    
    sem getWord =
    | MdProgram { } -> ""
    | MdLet {} -> "let"
    | MdLang {} -> "lang"
    | MdType {} -> "type"
    | MdUse {} -> "use"
    | MdSem {} -> "sem"
    | MdSyn {} -> "syn"
    | MdCon {} -> "con"
    | MdMexpr {} -> "mexpr"
    | MdWord { word = word } -> word

end
    
-- Takes a tree and returns the md file documenting the tree
-- Avoir un buffer qui preserve le long du parcours les commentaires sur des lignes consecutives reperés. Le vider si l'on croise un retour à la ligne dans un sep ou un word quelconque, le mettre dans la doc si l'on croise un noeud, et le vider juste après
let compileToMd =
    use TokenReader in use BreakerChooser in use MdObject in
    lam tree. lam repoPath.
    let changeExt : (String -> String -> String) = lam fileName. lam ext.
        match findiLast (eqc '.') fileName with Some i then
            concat (subsequence fileName 0 (addi 1 i)) ext
        else
            concat fileName (cons '.' ext)
    in

    let getId =
        lam kind. lam namespace. lam name.
        concatAll [kind, "#", namespace, "~", name] in

    recursive
    let sanitizePath = lam path.
        switch path
        case (['/'] ++ path) then cons '-' (sanitizePath path)
        case (['.'] ++ path) then cons '|' (sanitizePath path)
        case [x] ++ path then cons x (sanitizePath path)
        case [] then [] end in
    let removeComments =
        lam sons. filter (lam s. match s with Leaf (Comment {}, _) then false else true) sons in
    recursive
    let nthWord = lam sons. lam n.
        match sons with [Leaf (Word { content = word }, _), _] ++ rest then
            if eqi n 0 then (word, rest)
            else nthWord rest (subi n 1)
        else never in
    recursive
    let compileToMdRec : (DocTree -> String -> [String] -> ([String], Object)) =
    lam tree. lam namespace. lam currentComment.

        -- Takes in input the the main title of the output file, the namespace, the current comment buffer, the file path to the md output file, and all the sons.
        -- Start by open the output file 
        -- First emptying the comments buffer containing the global documentation.
        -- Will then recursivly goes into each son to collect each mdObjects.
        -- Collected objects will be sorted and displayed in the order: use / include, type, con, lang, syn, let / sem, mexpr
        -- Will return the new comment buffer, indeed, in some cases the bloc can end with some comments that are the documentation of the next bloc.
        let render : (String -> [String] -> String -> String -> [DocTree] -> [String]) =
            lam namespace. lam currentComment. lam mdTitle. lam mdFilePath. lam sons.
            -- Opening md file
            match fileWriteOpen (concat "doc-gen-output/" (changeExt mdFilePath "md")) with Some wc then
                let write = fileWriteString wc in

                -- Pushing title and global documentation
                write (concatAll ["# ", mdTitle, "\n\n"]);
                iter (lam c. write (concat c "\n<br>\n")) currentComment;

                -- Collecting objects
                let objects = foldl
                    (lam arg. lam s.
                    let obj = compileToMdRec s namespace arg.0 in
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
                            case MdInclude {} then { set with mdInclude = cons obj set.mdInclude }
                            case MdWord {} then set
                            end) objects
                        case [] then set
                        end
                    in buildSet { mdUse = [], mdLet = [], mdLang = [], mdType = [], mdSem = [], mdSyn = [], mdCon = [], mdMexpr = [], mdInclude = [] } objects.1 in


                -- Displaying uses and includes
                let displayUseInclude = lam title. lam arr.
                    let title = match arr with [] then "" else concatAll ["**", title, ":** \n<br>\n"] in
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

                displayUseInclude "Using" set.mdUse;
                displayUseInclude "Include" set.mdInclude;

                fileWriteClose wc;
                objects.0
            else error "Error writing to file."
        in
        match tree with Node { sons = sons, token = token, state = state } then
            let sons = filter (lam s. match s with Leaf (Separator {}, _) | Leaf (WeakComment {}, _)
            then false else true) sons in
            let sanitized = sanitizePath namespace in
            switch token case Word { content = content } then
            switch state
            case Program {} then
                let mdPath = sanitized in
                let mdTitle = content in
                recursive
                let extractProgramComments = lam sons.
                    match sons with [Leaf ( Comment { content = content }, _)] ++ rest then
                        let output = extractProgramComments rest in
                        (cons content output.0, output.1)
                    else ([], sons) in
                let extracted = extractProgramComments sons in
                render namespace extracted.0 mdTitle mdPath extracted.1;
                ([], MdProgram { filePath = mdTitle, doc = extracted.0 })
            case (Use {} | TopUse {}) then
                match nthWord sons 0 with (content, _) then
                    ([], MdUse { l = content })
                else never
            case (Let {} | TopLet {}) then
                match nthWord sons 0 with (name, rest) then
                    print (concatAll ["let: ", name, "\n"]);
                    let obj = MdLet { name = name, doc = currentComment } in
                    let commentBuffer = render
                        (concatAll [namespace, "-", name]) currentComment name
                        (getId "Let" sanitized name) rest in
                    (commentBuffer, obj)
                else never
            case (Type {} | TopType {}) then
                match nthWord sons 0 with (content, typedef) then
                    error "TODO: Type"
                else never 
            case Sem {} then
                error "TODO: Sem"
            case Syn {} then
                error "TODO: Syn"
            case (Con {} | TopCon {}) then
                error "TODO: Con"
            case Mexpr {} then
                error "TODO: Mexpr"
            case Lang {} then
                match nthWord sons 0 with (name, rest) then
                    print (concatAll ["lang: ", name, "\n"]);
                    let obj = MdLang { name = name, doc = currentComment } in
                    let commentBuffer = render
                        (concatAll [namespace, "-", name]) currentComment name
                        (getId "Lang" sanitized name) rest in
                    (commentBuffer, obj)
                else never
            end
            end
    
        else match tree with Leaf (token, state) then
            let w = MdWord { word = lit token } in
            match token with Comment { content = content } then
                (cons content currentComment, w)
            else match token with Include { content = content } then
                let path = (concatAll [dirname namespace, "/", content]) in
                printLn path;
                switch parse path
                case Some tree then
                    match compileToMdRec tree path [] with
                    ([], MdProgram { filePath = filePath })
                    then ([], MdInclude { filePath = filePath })
                    else never
                case None {} then
                    printLn (concat "WARNING: Failed to open file " path);
                    ([], MdInclude { filePath = path })
                end
            else
                ([], w) -- Emptying the buffer
        else never
    in
    match tree with Node { token = Word { content = content }, state = Program {} } then
        let res = compileToMdRec tree content [] in ()
    else error "The top node of the tree should be a Program."
