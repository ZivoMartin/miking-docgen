-- This file implement the compiler
-- Takes in input a doctree and generate all the md files

include "util.mc"
include "md-object.mc"    
include "../parser.mc"
include "../doc-tree.mc"
include "ext/file-ext.mc"
include "fileutils.mc"
include "renderer.mc"
    
-- Takes a tree and returns the md file documenting the tree
-- Avoir un buffer qui preserve le long du parcours les commentaires sur des lignes consecutives reperés. Le vider si l'on croise un retour à la ligne dans un sep ou un word quelconque, le mettre dans la doc si l'on croise un noeud, et le vider juste après
let compileToMd =
    use TokenReader in use BreakerChooser in use MdObject in
    lam tree. lam repoPath.
    recursive
    let compileToMdRec : Compiler =
    lam tree. lam namespace. lam currentComment.

        let render = lam namespace. lam comments. lam title. lam path. lam sons.
            render compileToMdRec namespace comments title path sons in
        
        match tree with Node { sons = sons, token = token, state = state } then
            
            let sanitized = sanitizePath namespace in
            let getId = getId sanitized in
            switch token case Word { content = content } then
            switch state
            case Program {} then
                let mdPath = sanitized in
                let mdTitle = content in
                recursive
                let extractProgramComments = lam sons.
                    match sons with [Leaf { token = Comment { content = content } }] ++ rest then
                        let output = extractProgramComments rest in
                        (cons content output.0, output.1)
                    else ([], sons) in
                let extracted = extractProgramComments sons in
                render namespace extracted.0 mdTitle mdPath extracted.1;
                ([], MdProgram { filePath = mdTitle, doc = (reverse extracted.0), link = mdPath })
            case (Use {} | TopUse {}) then
                match nthWord sons 0 with Some (content, _) then
                    ([], MdUse { l = content, link = "" })
                else never
            case (Let {} | TopLet {}) then
                match nthWord sons 0 with Some (name, rest) then
                    let id = getId name in
                    let obj = MdLet { name = name, doc = (reverse currentComment), link = id } in
                    let commentBuffer = render
                        (concatAll [namespace, "-", name]) currentComment name id rest in
                    (commentBuffer, obj)
                else never
            case (Type {} | TopType {}) then
                match nthWord sons 0 with Some (name, typedef) then
                    let t = match nthWord typedef 0 with Some ("=", typedef) then
                        Some (extractType typedef)
                    else None {} in
                    let id = getId name in    
                    let obj = MdType { name = name, t = t, doc = (reverse currentComment), link = id } in
                    let commentBuffer = render
                        (concatAll [namespace, "-", name]) currentComment name id [] in
                    (commentBuffer, obj)
                else never 
            case Sem {} then
                match nthWord sons 0 with Some (name, rest) then
                    let id = getId name in
                    let obj = MdSem { name = name, doc = (reverse currentComment),
                                      langName = extractLastNamespaceElement namespace, link = id  } in
                    let commentBuffer = render
                        (concatAll [namespace, "-", name]) currentComment name id rest in
                    (commentBuffer, obj)
                else never
            case Syn {} then
                match nthWord sons 0 with Some (name, rest) then
                    let id = getId name in
                    let obj = MdSyn { name = name, doc = (reverse currentComment),
                                      langName = extractLastNamespaceElement namespace, link = id } in
                    let commentBuffer = render
                        (concatAll [namespace, "-", name]) currentComment name id rest in
                    (commentBuffer, obj)
                else never
            case (Con {} | TopCon {}) then
                match nthWord sons 0 with Some (name, sons) then
                    recursive let getType = lam sons.
                        match nthWord sons 0 with
                        Some ("use", sons) then
                            match (nthWord sons 1) with Some (_, sons) then
                                getType sons else never
                        else extractType sons
                    let t = match nthWord sons 0 with Some (":", typedef) then
                        extractType typedef else never in
                    let id = getId name in
                    let obj = MdCon { name = name, doc = (reverse currentComment), t = t, link = id  } in
                    let commentBuffer = render
                        (concatAll [namespace, "-", name]) currentComment name id sons in
                    (commentBuffer, obj)
                else never
            case Mexpr {} then
                let id = getId "mexpr" in
                let obj = MdMexpr { doc = (reverse currentComment), link = id } in
                let commentBuffer = render
                    (concatAll [namespace, "-", "mexpr"]) currentComment "mexpr" id sons in
                (commentBuffer, obj)
            case Lang {} then
                match nthWord sons 0 with Some (name, rest) then
                    recursive let extractParents = lam words.
                        match nthWord words 0 with Some (w, words) then
                            switch w
                            case "end" | "type" | "sem" | "syn" | "con" then []
                            case name then
                                match nthWord words 0 with Some (_, words) then 
                                    cons name (extractParents words)
                                else [name]
                            end
                        else [] in
                    let id = getId name in    
                    let obj = MdLang { name = name, doc = (reverse currentComment),
                                        parents = extractParents rest, link = id } in
                    let commentBuffer = render
                        (concatAll [namespace, "-", name]) currentComment name id rest in
                    (commentBuffer, obj)
                else never
            end
            end
        else match tree with Leaf { token = token, state = state } then
            let w = MdWord { word = lit token } in
            switch token
            case Comment { content = content } then
                (cons content currentComment, w)
            case Include { content = content } then
                let path = (concatAll [dirname namespace, "/", content]) in
                printLn path;
                switch parse path
                case Some tree then
                    match compileToMdRec tree path [] with
                    ([], MdProgram { filePath = filePath })
                    then
                        let id = sanitizePath filePath in
                        ([], MdInclude { filePath = content, link = id })
                    else never
                case None {} then
                    printLn (concat "WARNING: Failed to open file " path);
                    ([], MdInclude { filePath = path, link = "" })
                end
            case Separator {} then
                (currentComment, w)
            case _ then
                ([], w) -- Emptying the buffer
            end
        else never
    in
    match tree with Node { token = Word { content = content }, state = Program {} } then
        let res = compileToMdRec tree content [] in ()
    else error "The top node of the tree should be a Program."
