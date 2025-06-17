-- This file implement the compiler
-- Takes in input a doctree and generate all the md files

include "parser.mc"
include "doc-tree.mc"
include "ext/file-ext.mc"
include "fileutils.mc"

lang MdObject

syn Object = 
    | MdProgram { filePath: String }
    | MdLet {}
    | MdLang {}
    | MdType { name: String, t: String }
    | MdUse { l: String }
    | MdSem {}
    | MdSyn {}
    | MdCon {}
    | MdMexpr {}
    | MdWord { word: String }
    | MdInclude { filePath: String }

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
            concat fileName ext
    in


    let removeComments =
        lam sons. filter (lam s. match s with Leaf (Comment {}, _) then false else true) sons in
    
    
    recursive
    let compileToMdRec : (DocTree -> String -> [String] -> ([String], Object)) =
    lam tree. lam currentPath. lam currentComment.
        let render : (String -> [String] -> (String ->()) -> [DocTree] -> ()) =
            lam currentPath. lam currentComment. lam write. lam sons.
            let objects = foldl
                (lam arg. lam s.
                let obj = compileToMdRec s currentPath arg.0 in
                (obj.0, cons obj.1 arg.1))
                (currentComment, [])
                sons in
            () in

        match tree with Node { sons = sons, token = token, state = state } then
            let sons = filter (lam s. match s with Leaf (Separator {}, _) | Leaf (WeakComment {}, _)
            then false else true) sons in
            switch token case Word { content = content } then
            switch state
            case Program {} then 
                match fileWriteOpen (changeExt content "md") with Some wc then
                    let write = fileWriteString wc in
                    write (concatAll ["# ", content, "\n\n"]);
                    recursive
                    let extractProgramComments = lam sons.
                        match sons with [Leaf ( Comment { content = content }, _)] ++ rest then
                            write (concat content "\n<br>\n");
                            extractProgramComments rest
                        else sons in
                    let sons = extractProgramComments sons in
                    render currentPath currentComment write sons;
                    fileWriteClose wc;
                    ([], MdProgram { filePath = content })
                else error "Error writing to file."
            case (Use {} | TopUse {}) then
                let sons = removeComments sons in
                match head sons with Leaf (Word { content = content }, _) then
                    ([], MdUse { l = content })
                else never
            case (Let {} | TopLet {}) then
                error "TODO"
            case (Type {} | TopType {}) then
                let sons = removeComments sons in
                match sons with [Leaf (Word { content = name }, _), _] ++ typedef then
                    error "TODO: Type"
                else never 
            case Sem {} then
                error "TODO"
            case Syn {} then
                error "TODO"
            case (Con {} | TopCon {}) then
                error "TODO"
            case Mexpr {} then
                error "TODO"
            case Lang {} then
                error "TODO"
            end
            end
    
        else match tree with Leaf (token, state) then
            let w = MdWord { word = lit token } in
            match token with Comment { content = content } then
                (cons content currentComment, w)
            else match token with Include { content = content } then
                let path = (concatAll [dirname currentPath, "/", content]) in
                printLn path;
                let tree = parse path in
                match compileToMdRec tree path [] with ([], MdProgram { filePath = filePath }) then
                error "TODO" else never
            else
                ([], w) -- Emptying the buffer
        else never
    in
    match tree with Node { token = Word { content = content }, state = Program {} } then
        let res = compileToMdRec tree content [] in ()
    else error "The top node of the tree should be a Program."
