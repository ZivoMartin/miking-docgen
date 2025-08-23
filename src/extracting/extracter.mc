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
--      * It is a separator with less than 2 newlines -> we simply keep the buffer unchanged.  
--        This is crucial, for example in cases where the documented object is indented.  
--        In that case, the documentation (i.e., the comments) is likely also indented and separated by separators.  
--        However, if the separator **contains** more than one newline, we must still clear the buffer, because it represents a break.  
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
include "../global/logger.mc"
        
-- Takes a tree and builds the objects
-- Comment buffer tracks consecutive comments between tokens
-- If a newline separator is hit, the buffer is cleared
let extract : DocTree -> ObjectTree =
    use TokenReader in use BreakerChooser in use ObjectKinds in
    lam tree.
    extractingLog "Beggining of extraction...";

     -- Entry point: tree must be Program node
    match tree with Node { token = TokenProgram { content = content, includeSet = includeSet }, state = Program {} } then
    let prefix = includeSetPrefix includeSet in
    
    -- Buffer of collected comments
    type CommentBuffer = [String] in

    -- Output of one extractRec step
    type ExtractRecOutput = { obj: Option ObjectTree, commentBuffer: CommentBuffer, sourceCodeBuilder: SourceCodeBuilder, utestCount: Int } in

    recursive
    let extractRec : (DocTree -> String -> CommentBuffer -> SourceCodeBuilder -> Bool -> Int -> ExtractRecOutput ) =
    lam tree. lam namespace. lam commentBuffer. lam sourceCodeBuilder. lam inStdlib. lam utestCount.

        let shouldClear : String -> Bool = lam content. gti (count (eqChar '\n') content) 1 in
        let sourceCodeBuilder = absorbWord sourceCodeBuilder tree in

        let defaultObject = lam namespace. lam isStdlib.
            let defaultObject = objWithNamespace defaultObject namespace in
            let defaultObject = objWithIsStdlib defaultObject isStdlib in
            if isStdlib then defaultObject else objWithPrefix defaultObject prefix
        in
    
        switch tree 
        case Node { sons = sons, token = token, state = state } then

            -- Builds doc string from comments
            let buildDoc : [String] -> String = lam commentBuffer.
                let res = strJoin "  \n" (map (lam s. if strStartsWith " " s then s else cons ' ' s) commentBuffer) in
                match res with "" then "No documentation available here." else res in

            let finish : Object -> SourceCodeBuilder -> { builder: SourceCodeBuilder, obj: Object } = lam obj. lam sourceCodeBuilder.
                let sourceCode = finish sourceCodeBuilder in
                { obj = { obj with sourceCode = sourceCode.sourceCode }, builder = sourceCode.builder } in

            let obj = objWithIsStdlib (defaultObject namespace inStdlib) inStdlib in
            let doc = buildDoc (reverse commentBuffer) in

            -- Process children nodes
            let process : State -> [DocTree] -> String -> String -> String -> ObjectKind -> Int -> ExtractRecOutput =
                lam state. lam sons. lam name. lam namespace. lam doc. lam kind. lam utestCount.
                type Arg = { sons: [ObjectTree], ctx: ExtractRecOutput } in
                let foldResult = foldl
                    (lam arg: Arg. lam s: DocTree.
                        let ctx = arg.ctx in
                        let ctx = extractRec s namespace ctx.commentBuffer ctx.sourceCodeBuilder inStdlib ctx.utestCount in
                        let sons = match ctx.obj with Some obj then cons obj arg.sons else arg.sons in
                        { sons = sons, ctx = ctx })
                    { sons = [], ctx = { commentBuffer = [], sourceCodeBuilder = sourceCodeBuilder, utestCount = utestCount, obj = None {} } }
                    sons in
                let obj = objWithNamespace obj namespace in
                let obj = { obj with name = name, kind = kind, doc = doc } in
                match finish obj foldResult.ctx.sourceCodeBuilder with { obj = obj, builder = sourceCodeBuilder } in
                let obj = ObjectNode { obj = obj, sons = reverse foldResult.sons } in
                { foldResult.ctx with obj = Some obj, sourceCodeBuilder = sourceCodeBuilder } in

            -- Dispatch by token type + state
            switch token case TokenWord { content = content } | TokenProgram { content = content } then
            switch state
            case Program {} then
                recursive
                let extractProgramComments = lam sons.
                    switch sons
                    case [Leaf { token = TokenComment { content = content } | TokenMultiLineComment { content = content } }] ++ rest then
                        let output = extractProgramComments rest in
                        { output with comments = cons content output.comments }
                    case [Leaf { token = TokenSeparator { content = content } }] ++ rest then
                        if shouldClear content then { comments = [], sons = sons }
                        else extractProgramComments rest
                    case _ then { comments = [], sons = sons }
                    end
                in
                let extractRes = extractProgramComments sons in
                process state sons content content
                    (buildDoc extractRes.comments)
                    (ObjProgram {})
                    utestCount

            case Mexpr {} then
                process state sons "mexpr" (getNamespace namespace "mexpr" "") doc (ObjMexpr {}) utestCount

            case (Use {} | TopUse {}) then
                let name = getName sons in
                let obj = { obj with name = name.word, kind = ObjUse {} } in
                let sourceCodeBuilder = foldl absorbWord sourceCodeBuilder sons in
                match finish obj sourceCodeBuilder with { obj = obj, builder = sourceCodeBuilder } in
                { obj = Some (ObjectNode { obj = obj, sons = [] }), commentBuffer = [], sourceCodeBuilder = sourceCodeBuilder, utestCount = utestCount }

            case TopUtest {} | Utest {} then
                let name = int2string utestCount in
                process state sons name (getNamespace namespace name "utest") doc (ObjUtest {}) (addi utestCount 1)
            case Rec {} | TopRec {} then
                process state sons "" namespace doc (ObjRecursiveBlock {}) utestCount 
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
                    case (Let {} | TopLet {} | RecLet {}) then
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
                let namespace = getNamespace namespace name.word (getFirstWord kind) in
                process state sons name.word namespace doc kind utestCount
                end
            case _ then
                error (concat "Not covered: " (toString state))
            end
        case Leaf { token = token, state = state } then
            let defaultRes = { commentBuffer = [], sourceCodeBuilder = sourceCodeBuilder, obj = None {}, utestCount = utestCount } in
            -- Leaf dispatch
            switch token
            case TokenComment { content = content } | TokenMultiLineComment { content = content }then
                { defaultRes with commentBuffer = cons content commentBuffer }
            case TokenSeparator { content = content } then
                -- Clear comment buffer if more than  one \n found
                if shouldClear content then defaultRes
                else { defaultRes with commentBuffer = commentBuffer }
            case TokenStr {} | TokenWord {} | TokenRecursiveEnder {} then defaultRes
            end
        case IncludeNode  { token = TokenInclude { content = content }, state = state, tree = tree, path = path, isStdlib = isStdlib } then
            -- Load included file
            let defaultObject = defaultObject path isStdlib in
            let defaultObject = objWithKind defaultObject (ObjInclude { pathInFile = content }) in
            let defaultObject = objWithName defaultObject path in

            let emptyInclude = ObjectNode { obj = defaultObject, sons = [] } in

            let defaultRes = { commentBuffer = [], sourceCodeBuilder = sourceCodeBuilder, obj = Some emptyInclude, utestCount = utestCount } in
            match tree with Some tree then
                extractingLog (concat "Extracting on: " path);
                let res = extractRec tree path [] (newSourceCodeBuilder ()) isStdlib utestCount in
                match res with { obj = Some (ObjectNode { obj = progObj, sons = sons } & progObjTree) } then
                    let includeObj = { progObj with isStdlib = isStdlib, kind = ObjInclude { pathInFile = content }, sourceCode = sourceCodeEmpty () } in
                    { defaultRes with obj = Some (ObjectNode { obj = includeObj, sons = [ progObjTree ] })  }
                else
                    extractingWarn "Found a leaf at the root of a Program"; defaultRes
            else defaultRes
        end
    in

    let obj = match (extractRec tree content [] (newSourceCodeBuilder ()) false 0).obj with Some obj then
        obj
    else
        error "Extraction failed: extractRec returned None" in
    
    obj

    else error "Extraction failed: the top node of the output tree should always be a program."
