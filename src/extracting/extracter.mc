-- # Extracter: generates ObjectTree from DocTree
--
-- This module implements the extracter:
-- - input: a parsed `DocTree`
-- - output: an `ObjectTree` representing documentation content
--
-- The parsing phase produces a `DocTree`, which is a relatively minimal structure that only defines the layout of the code.
-- In the extracting phase, we transform this `DocTree` into an `Object`.  
-- Objects also follow a tree logic, with sub-objects, but they carry more semantic information than the `DocTree`.
--
-- In this phase, look-ahead is allowed, meaning we can inspect as many upcoming nodes or children/subchildren of the current node as we wish.
--
-- Here is the list of information contained in an `Object`:
--
--  - `name`: The name of the object. If the object is a `let`, it corresponds to the variable name.  
--    To extract it, we perform a look-ahead on the children of the current node until we find the object’s name.
--
--  - `doc`: The documentation is one of the most important components.  
--    It consists of the comments written above the definition of an object.  
--    To retrieve it, we maintain a comment buffer throughout the extraction process.  
--    Each time we encounter a comment, it is added to the buffer. If we encounter something other than a comment, there are three cases:
--      * It is a `DocTree` node -> the current state of the buffer becomes the documentation for that node, and the buffer is cleared.
--      * It is a separator **without** a newline -> we simply keep the buffer unchanged.  
--        This is crucial, for example in cases where the documented object is indented.  
--        In that case, the documentation (i.e., the comments) is likely also indented and separated by separators.  
--        However, if the separator **contains** a newline, we must still clear the buffer, because it represents a break.  
--      * It is anything else -> we clear the buffer.
--
--  - `namespace`: The namespace reflects the current position of the node in the tree.  
--    Each namespace is naturally unique and is used, combined with the object name, to produce unique identifiers for every object.  
--    Duplicate names within the same namespace are handled elsewhere.
--
--  - `kind`: The `kind` field is specific to the object’s type.  
--    It is first used to distinguish between object types (`Let`, `Sem`, etc.),  
--    but also to enrich objects with category-specific metadata:  
--    e.g., the composition of a language, parameter names of a let, or the variants of a syntax.
--
--  - `sourceCode`: The `sourceCode` of an object is an **absolute** representation of the object’s source code.  
--    It is not just a plain string, but a structured value defined in `source-code-word.mc` and `source-code-builder.mc`.  
--    The idea is to pass each node and leaf of the `DocTree` to the source code builder,  
--    and call its `finish` function after all children have been processed to obtain the final representation.
    
include "../parsing/parser.mc"
include "../parsing/doc-tree.mc"

include "fileutils.mc"

include "./util.mc"
include "./objects.mc"
include "./source-code-builder.mc"
include "../logger.mc"
        
-- Takes a tree and builds the objects
-- Comment buffer tracks consecutive comments between tokens
-- If a newline separator is hit, the buffer is cleared
let extract : DocTree -> ObjectTree =
    use TokenReader in use BreakerChooser in use ObjectKinds in
    lam tree.
    extractingLog "Beggining of extraction...";
    

    -- Buffer of collected comments
    type CommentBuffer = [String] in

    -- Output of one extractRec step
    type ExtractRecOutput = { obj: ObjectTree, commentBuffer: CommentBuffer, sourceCodeBuilder: SourceCodeBuilder, utestCount: Int } in

    recursive
    let extractRec : (DocTree -> String -> CommentBuffer -> SourceCodeBuilder -> Bool -> Int -> ExtractRecOutput ) =
    lam tree. lam namespace. lam commentBuffer. lam sourceCodeBuilder. lam inStdlib. lam utestCount.
    
        let sourceCodeBuilder = absorbWord sourceCodeBuilder tree in
        switch tree 
        case Node { sons = sons, token = token, state = state } then

            -- Builds doc string from comments
            let buildDoc : [String] -> String = lam commentBuffer.
                let res = strJoin "  \n" (map (lam s. if strStartsWith " " s then s else cons ' ' s) commentBuffer) in
                match res with "" then "No documentation available here." else res in

            let finish : Object -> SourceCodeBuilder -> { builder: SourceCodeBuilder, obj: Object } = lam obj. lam sourceCodeBuilder.
                let sourceCode = finish sourceCodeBuilder in
                { obj = { obj with sourceCode = sourceCode.sourceCode }, builder = sourceCode.builder } in
    
            -- Start new object
            let obj = { defaultObject with namespace = namespace } in
            let doc = buildDoc (reverse commentBuffer) in
            

            -- Process children nodes
            let process : State -> [DocTree] -> String -> String -> String -> ObjectKind -> Int -> ExtractRecOutput =
                lam state. lam sons. lam name. lam namespace. lam doc. lam kind. lam utestCount.
                type Arg = { sons: [ObjectTree], ctx: ExtractRecOutput } in
                let foldResult = foldl
                    (lam arg: Arg. lam s: DocTree.
                        let ctx = arg.ctx in
                        let ctx = extractRec s namespace ctx.commentBuffer ctx.sourceCodeBuilder inStdlib ctx.utestCount in
                        { sons = cons ctx.obj arg.sons, ctx = ctx })
                    { sons = [], ctx = { commentBuffer = [], sourceCodeBuilder = sourceCodeBuilder, utestCount = utestCount, obj = ObjectLeaf "" } }
                    sons in
                let obj = { obj with name = name, kind = kind, doc = doc } in
                match finish obj foldResult.ctx.sourceCodeBuilder with { obj = obj, builder = sourceCodeBuilder } in
                let obj = ObjectNode { obj = obj, sons = reverse foldResult.sons } in
                { foldResult.ctx with obj = obj, sourceCodeBuilder = sourceCodeBuilder } in

            -- Dispatch by token type + state
            switch token case Word { content = content } | Recursive { lit = content } | ProgramToken { content = content } then
            switch state
            case Program {} then
                recursive
                let extractProgramComments = lam sons.
                    match sons with [Leaf { token = Comment { content = content } }] ++ rest then
                        let output = extractProgramComments rest in
                        { output with comments = cons content output.comments }
                    else { comments = [], sons = sons } in
                let extractRes = extractProgramComments sons in
                process state sons content content
                    (buildDoc extractRes.comments)
                    (ObjProgram { isStdlib = inStdlib })
                    utestCount

            case Mexpr {} then
                process state sons "mexpr" (getNamespace namespace "mexpr") doc (ObjMexpr {}) utestCount

            case (Use {} | TopUse {}) then
                let name = getName sons in
                let obj = { obj with name = name.word, kind = ObjUse {} } in
                let sourceCodeBuilder = foldl absorbWord sourceCodeBuilder sons in
                match finish obj sourceCodeBuilder with { obj = obj, builder = sourceCodeBuilder } in
                { obj = ObjectNode { obj = obj, sons = [] }, commentBuffer = [], sourceCodeBuilder = sourceCodeBuilder, utestCount = utestCount }

            case TopUtest {} | Utest {} then
                let name = int2string utestCount in
                process state sons name (getNamespace namespace name) doc (ObjUtest {}) (addi utestCount 1)
    
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
                    case (Let {} | TopLet {} | Rec {} | TopRec {}) then
                        let rec = match state with Let {} | TopLet {} then false else true in
                        let sons = goToEqual sons in
                        let sons = skipUseIn sons in
                        -- Extract params if any
                        let args = extractParams sons in
                        ObjLet { rec = rec, args = args, ty = None {} }
                    case Sem {} then
                        ObjSem { langName = extractLastNamespaceElement namespace, variants = extractVariants (goToEqual sons), ty = None {} }
                    case Syn {} then ObjSyn { langName = extractLastNamespaceElement namespace, variants = extractVariants (goToEqual sons) }
                    case Lang {} then ObjLang { parents = extractParents name.rest }
                    case (Con {} | TopCon {}) then
                        let t =
                            match nthWord name.rest 0 with Some { word = ":", rest = typedef } then
                                extractType (skipUseIn typedef)
                            else
                                extractingWarn (join ["The constructor ", name.word, " is typeless."]);
                                ""
                        in
                        ObjCon { t = t }

                    case (Type {} | TopType {}) then
                        let t = match nthWord name.rest 0 with Some { word = "=", rest = typedef } then
                                Some (extractType typedef) else None {} in
                        ObjType { t = t }

                    end in
                process state sons name.word (getNamespace namespace name.word) doc kind utestCount
                end
            case _ then
                error (concat "Not covered: " (toString state))
            end
        case Leaf { token = token, state = state } then
            let w = ObjectLeaf (lit token) in
            let defaultRes = { commentBuffer = [], sourceCodeBuilder = sourceCodeBuilder, obj = w, utestCount = utestCount } in
            -- Leaf dispatch
            switch token
            case Comment { content = content } then
                { defaultRes with commentBuffer = cons content commentBuffer }
            case Separator { content = content } then
                -- Clear comment buffer if \n found
                if strContains content '\n' then defaultRes
                else { defaultRes with commentBuffer = commentBuffer }

            case WeakComment {} | Str {} | Word {} then defaultRes
            end
        case IncludeNode  { token = Include { content = content }, state = state, tree = tree, path = path, isStdlib = isStdlib } then
            -- Load included file
            let emptyInclude = ObjectNode { obj = { defaultObject with kind = ObjInclude { isStdlib = isStdlib, pathInFile = content }, name = path, namespace = path }, sons = [] } in    
            let defaultRes = { commentBuffer = [], sourceCodeBuilder = sourceCodeBuilder, obj = emptyInclude, utestCount = utestCount } in
            match tree with Some tree then
                extractingLog (concat "Extracting on: " path);
                let res = extractRec tree path [] (newSourceCodeBuilder ()) isStdlib utestCount in
                match res with
                    { obj = (ObjectNode { obj = progObj, sons = sons } & progObjTree) } then
                    let includeObj = { progObj with kind = ObjInclude { isStdlib = isStdlib, pathInFile = content } } in
                    { defaultRes with obj = ObjectNode { obj = includeObj, sons = [ progObjTree ] }  }
                else
                    extractingWarn (join ["Found a leaf at the root of a Program : ", objTreeToString res.obj]); defaultRes
            else defaultRes
        end
    in

    -- Entry point: tree must be Program node
    match tree with Node { token = ProgramToken { content = content }, state = Program {} } then
        (extractRec tree content [] (newSourceCodeBuilder ()) false 0).obj
    else error "Extraction failed: the top node of the output tree should always be a program."
