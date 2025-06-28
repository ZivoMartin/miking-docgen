-- # Extracter: generates ObjectTree from DocTree
--
-- This module implements the extracter:
-- - input: a parsed `DocTree`
-- - output: an `ObjectTree` representing documentation content
--
-- The extracter walks the `DocTree`:
-- - It accumulates comments across lines (using a comment buffer)
-- - It builds the corresponding Object and ObjectTree
-- - It detects Includes and recursively loads included files
--
-- The result can later be rendered as Markdown / HTML files.

include "util.mc"
include "objects.mc"
include "source-code-builder.mc"
    
include "../parsing/parser.mc"
include "../parsing/doc-tree.mc"

include "fileutils.mc"
include "hashmap.mc"

-- Takes a tree and builds the objects
-- Comment buffer tracks consecutive comments between tokens
-- If a newline separator is hit, the buffer is cleared
let extract : DocTree -> ObjectTree =
    use TokenReader in use BreakerChooser in use ObjectKinds in
    lam tree.

    -- HashSet of included files
    type IncludeSet = HashMap String () in

    -- Buffer of collected comments
    type CommentBuffer = [String] in

    -- Output of one extractRec step
    type ExtractRecOutput = { obj: ObjectTree, commentBuffer: CommentBuffer, includeSet: IncludeSet, sourceCodeBuilder: SourceCodeBuilder, utestCount: Int } in

    recursive
    let extractRec : (DocTree -> String -> CommentBuffer -> SourceCodeBuilder -> IncludeSet -> Bool -> Int -> ExtractRecOutput ) =
    lam tree. lam namespace. lam commentBuffer. lam sourceCodeBuilder. lam includeSet. lam inStdlib. lam utestCount.
        let sourceCodeBuilder = absorbWord sourceCodeBuilder tree in
        switch tree 
        case Node { sons = sons, token = token, state = state } then

            -- Builds doc string from comments
            let buildDoc : [String] -> String = lam commentBuffer.
                let res = strJoin "  \n" (reverse commentBuffer) in
                match res with "" then "No documentation available here." else res in

            let finish : Object -> SourceCodeBuilder -> { builder: SourceCodeBuilder, obj: Object } = lam obj. lam sourceCodeBuilder.
                let sourceCode = finish sourceCodeBuilder in
                { obj = { obj with sourceCode = sourceCode.sourceCode }, builder = sourceCode.builder } in
    
            -- Start new object
            let obj = { defaultObject with namespace = namespace } in
            let doc = buildDoc commentBuffer in
            

            -- Process children nodes
            let process : [DocTree] -> String -> String -> String -> ObjectKind -> Int -> ExtractRecOutput =
                lam sons. lam name. lam namespace. lam doc. lam kind. lam utestCount.
                type Arg = { sons: [ObjectTree], ctx: ExtractRecOutput } in
                let foldResult = foldl
                    (lam arg: Arg. lam s: DocTree.
                        let ctx = arg.ctx in
                        let ctx = extractRec s namespace ctx.commentBuffer ctx.sourceCodeBuilder ctx.includeSet inStdlib ctx.utestCount in
                        { sons = cons ctx.obj arg.sons, ctx = ctx })
                    { sons = [], ctx = { commentBuffer = [], includeSet = includeSet, sourceCodeBuilder = sourceCodeBuilder, utestCount = utestCount, obj = ObjectLeaf "" } }
                    sons in
                let obj = { obj with name = name, kind = kind, doc = doc } in
                match finish obj foldResult.ctx.sourceCodeBuilder with { obj = obj, builder = sourceCodeBuilder } in
                let obj = ObjectNode { obj = obj, sons = foldResult.sons } in
                { foldResult.ctx with obj = obj, sourceCodeBuilder = sourceCodeBuilder } in

            -- Dispatch by token type + state
            switch token case Word { content = content } then
            switch state
            case Program {} then
                recursive
                let extractProgramComments = lam sons.
                    match sons with [Leaf { token = Comment { content = content } }] ++ rest then
                        let output = extractProgramComments rest in
                        { output with comments = cons content output.comments }
                    else { comments = [], sons = sons } in
                let extractRes = extractProgramComments sons in
                process sons content content
                    (buildDoc (reverse extractRes.comments))
                    (ObjProgram { isStdlib = inStdlib })
                    utestCount

            case Mexpr {} then
                process sons "mexpr" (getNamespace namespace "mexpr") doc (ObjMexpr {}) utestCount

            case (Use {} | TopUse {}) then
                let name = getName sons in
                let obj = { obj with name = name.word, kind = ObjUse {} } in
                let sourceCodeBuilder = foldl absorbWord sourceCodeBuilder sons in
                match finish obj sourceCodeBuilder with { obj = obj, builder = sourceCodeBuilder } in
                { obj = ObjectNode { obj = obj, sons = [] }, commentBuffer = [], sourceCodeBuilder = sourceCodeBuilder, includeSet = includeSet, utestCount = utestCount }

            case TopUtest {} | Utest {} then
                let name = int2string utestCount in
                process sons name (getNamespace namespace name) doc (ObjUtest {}) (addi utestCount 1)
    
            case state then
                -- Look for '=' in children
                recursive let goToEqual = lam sons.
                    switch nthWord sons 0
                    case Some { word = "=", rest = rest } then rest
                    case Some { rest = rest } then goToEqual rest
                    case None {} then []
                    end in

                let name = getName sons in
                let kind = switch state
                    case (Let {} | TopLet {}) then
                        let sons = goToEqual sons in
                        let sons = skipUseIn sons in

                        -- Extract params if any
                        let args = extractParams sons in
                        ObjLet { rec = false, args = args }
                    case Sem {} then ObjSem { langName = extractLastNamespaceElement namespace, variants = extractVariants (goToEqual sons) }
                    case Syn {} then ObjSyn { langName = extractLastNamespaceElement namespace, variants = extractVariants (goToEqual sons) }
                    case Lang {} then ObjLang { parents = reverse (extractParents name.rest) }
                    case (Con {} | TopCon {}) then
                        let t =
                            match nthWord name.rest 0 with Some { word = ":", rest = typedef } then
                                extractType (skipUseIn typedef)
                            else
                                warn (concatAll ["The constructor ", name.word, " is typeless."]);
                                ""
                        in
                        ObjCon { t = t }

                    case (Type {} | TopType {}) then
                        let t = match nthWord name.rest 0 with Some { word = "=", rest = typedef } then
                                Some (extractType typedef) else None {} in
                        ObjType { t = t }

                end in
                process sons name.word (getNamespace namespace name.word) doc kind utestCount
             end end

        case Leaf { token = token, state = state } then
            let w = ObjectLeaf (lit token) in
            let defaultRes = { commentBuffer = [], sourceCodeBuilder = sourceCodeBuilder, obj = w, includeSet = includeSet, utestCount = utestCount } in
            -- Leaf dispatch
            switch token
            case Comment { content = content } then
                { defaultRes with commentBuffer = cons content commentBuffer }

            case Include { content = content } then
                -- Load included file
                match goHere (dirname namespace) content with { path = path, isStdlib = isStdlib } in
                let isStdlib = or inStdlib isStdlib in
                let emptyInclude = ObjectNode { obj = { defaultObject with kind = ObjInclude { isStdlib = isStdlib, pathInFile = content }, name = path, namespace = path }, sons = [] } in
    
                if hmMem path includeSet then
                    { defaultRes with obj = emptyInclude }
                else
                    let newIncludeSet = hmInsert path () includeSet in
                    match parse path with Some tree in
                    match extractRec tree path [] sourceCodeBuilder newIncludeSet isStdlib utestCount with
                        { commentBuffer = [], sourceCodeBuilder = sourceCodeBuilder, obj = (ObjectNode { obj = progObj, sons = sons } & progObjTree), includeSet = includeSet } in

                    let includeObj = { progObj with kind = ObjInclude { isStdlib = isStdlib, pathInFile = content } } in
                    { defaultRes with obj = ObjectNode { obj = includeObj, sons = [ progObjTree ] }, includeSet = includeSet  }
            case Separator { content = content } then
                -- Clear comment buffer if \n found
                if strContains content '\n' then defaultRes
                else { defaultRes with commentBuffer = commentBuffer }

            case _ then defaultRes
            end
        end
    in

    -- Entry point: tree must be Program node
    match tree with Node { token = Word { content = content }, state = Program {} } then
        (extractRec tree content [] (newSourceCodeBuilder ()) (hashmapEmpty ()) false 0).obj
    else error "Extraction failed: the top node of the output tree should always be a program."
