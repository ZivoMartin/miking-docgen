-- This file implement the extracter
-- Takes in input a doctree and generate all the md files

include "util.mc"
include "objects.mc"    
include "../parsing/parser.mc"
include "../parsing/doc-tree.mc"
include "ext/file-ext.mc"
include "fileutils.mc"
include "hashmap.mc"
    
-- Takes a tree and build the md object 
-- Avoir un buffer qui preserve le long du parcours les commentaires sur des lignes consecutives reperés. Le vider si l'on croise un retour à la ligne dans un sep ou un word quelconque, le mettre dans la doc si l'on croise un noeud, et le vider juste après
let extract : DocTree -> ObjectTree =
    use TokenReader in use BreakerChooser in use ObjectKinds in
    lam tree.

    -- At the end of an extract call, the returned IncludeSet is an hashset containing all the paths in the projects.
    type IncludeSet = HashMap String () in
    type CommentBuffer = [String] in
    type ExtractRecOutput = { obj: ObjectTree, commentBuffer: CommentBuffer, includeSet: IncludeSet } in
    recursive
    let extractRec : (DocTree -> String -> CommentBuffer -> IncludeSet -> ExtractRecOutput) =
    lam tree. lam namespace. lam commentBuffer. lam includeSet.
        switch tree
        case Node { sons = sons, token = token, state = state } then

            let buildDoc : [String] -> String = lam commentBuffer.
                let commentBuffer = map strTrim commentBuffer in
                let res = strJoin "  \n" (reverse commentBuffer) in
                match res with "" then "No documentation available here." else res in

            let getName = lam sons. match nthWord sons 0 with Some r then r else never in

            let obj = { defaultObject with namespace = namespace } in
            let doc = buildDoc commentBuffer in

            let process : [DocTree] -> String -> String -> String -> ObjectKind -> ExtractRecOutput =
                lam sons. lam name. lam namespace. lam doc. lam kind.

                let foldResult = foldl
                    (lam arg. lam s.
                    let res = extractRec s namespace arg.commentBuffer arg.includeSet in
                    { commentBuffer = res.commentBuffer, sons = cons res.obj arg.sons, includeSet = res.includeSet })
                    { commentBuffer = [], sons = [], includeSet = includeSet }
                    sons in
                let obj = { obj with name = name, kind = kind, doc = doc } in
                let obj = ObjectNode { obj = obj, sons = foldResult.sons } in
                { obj = obj, commentBuffer = foldResult.commentBuffer, includeSet = foldResult.includeSet } in


            switch token case Word { content = content } then
            switch state
            case Program {} then
                recursive
                let extractProgramComments = lam sons.
                    match sons with [Leaf { token = Comment { content = content } }] ++ rest then
                        let output = extractProgramComments rest in
                        { output with comments = cons content output.comments }
                    else { comments = [], sons = sons } in
                match extractProgramComments sons with
                    { comments = extractedComments, sons = sons } then
                    process sons content content (buildDoc extractedComments) (ObjProgram {})
                else never
            case Mexpr {} then
                process sons "mexpr" (getNamespace namespace "mexpr") doc (ObjMexpr {})
            case (Use {} | TopUse {}) then
                let name = getName sons in
                let obj = { obj with name = name.word, kind = ObjUse {} } in
                { obj = ObjectNode { obj = obj, sons = [] }, commentBuffer = [], includeSet = includeSet }
            case state then
                let name = getName sons in
                let kind = switch state
                    case (Let {} | TopLet {}) then
                        let sons = recursive let goToEqual = lam sons.
                            switch nthWord sons 0
                            case Some { word = "=", rest = rest } then rest
                            case Some { rest = rest } then goToEqual rest
                            end in goToEqual sons in

                        let sons = skipUseIn sons in

                        let args = recursive let extractParams = lam sons.
                            switch nthWord sons 0 
                            case Some { word = "lam", rest = rest } then
                                match getName rest with { word = word, rest = rest} then
                                    match nthWord rest 0 with Some { rest = rest, word = w } then
                                        cons word (extractParams rest)
                                    else never
                                else never
                            case _ then []
                            end in extractParams sons in

                        ObjLet { rec = false, args = args }
                    case Sem {} then ObjSem { langName = extractLastNamespaceElement namespace }
                    case Syn {} then ObjSyn { langName = extractLastNamespaceElement namespace }
                    case Lang {} then ObjLang { parents = reverse (extractParents name.rest) }
                    case (Con {} | TopCon {}) then
                        let t = match nthWord name.rest 0 with Some { word = ":", rest = typedef } then
                                extractType (skipUseIn typedef) else never in
                        ObjCon { t = t }
                    case (Type {} | TopType {}) then
                        let t = match nthWord name.rest 0 with Some { word = "=", rest = typedef } then
                                Some (extractType typedef) else None {} in
                        ObjType { t = t } end in
                process sons name.word (getNamespace namespace name.word) doc kind
             end end
        case Leaf { token = token, state = state } then
            let w = ObjectLeaf (lit token) in
            switch token
            case Comment { content = content } then
                { commentBuffer = cons content commentBuffer, obj = w, includeSet = includeSet }
            case Include { content = content } then
                let path = goHere (dirname namespace) content in
                let path = normalizePath path in
                let emptyInclude = ObjectNode { obj = { defaultObject with kind = ObjInclude {}, name = path, namespace = path }, sons = [] } in
        
                if hmMem path includeSet then { commentBuffer = [], obj = emptyInclude, includeSet = includeSet } else

                let newIncludeSet = hmInsert path () includeSet in
                printLn path;
                switch parse path
                case Some tree then
                    match extractRec tree path [] newIncludeSet with { commentBuffer = [], obj = (ObjectNode { obj = progObj, sons = sons } & progObjTree), includeSet = includeSet } then
                        let includeObj = { progObj with kind = ObjInclude {} } in
                        { commentBuffer = [], obj = ObjectNode { obj = includeObj, sons = [ progObjTree ] }, includeSet = includeSet }
                    else never
                case None {} then
                    printLn (concat "WARNING: Failed to open file " path);
                    { commentBuffer = [], obj = emptyInclude, includeSet = newIncludeSet }
                end
            case Separator { content = content } then
                if strContains content '\n' then { commentBuffer = [], obj = w, includeSet = includeSet }
                else { commentBuffer = commentBuffer, obj = w, includeSet = includeSet }
    
            case _ then { commentBuffer = [], obj = w, includeSet = includeSet }
            end
        end
    in
    match tree with Node { token = Word { content = content }, state = Program {} } then
        let res = extractRec tree content [] (hashmapEmpty ()) in res.obj
    else error "The top node of the tree should be a Program."
