-- # Parser: building a DocTree from a source file 
--
-- This module defines a parser that reads a file and produces a `DocTree`, a hierarchical
-- structure of tokens annotated with formatting state.
--
-- We are trying to segment the code using a markup system. Certain specific words open nodes in the tree, and some words close nodes.
-- Take the example ```let x = 3 * 8 in```. Here, let opens a node—more precisely, a node of type let. At the moment the node is opened,
-- we need to decide which word will close this block.
-- In Miking's syntax, several words can potentially close a let: in, lang, mexpr, and many others.
-- In our example, when the parser encounters in, it recognizes ```in``` as one of the breaker and therefore closes the block.
--
-- The detection of opening words is handled in the parser, while the selection of breakers based on the current context is done in breaker-choosers.mc.
--
-- When a break occurs, there are three possible behaviors:
-- 1. Local break: we simply finish the current block.
--    Example: lang ... end -> 'end' only terminates the 'lang' block.
--
-- 2. Global break: we terminate not just the current block, but also one or more parent blocks.
--    Example: lang ... sem ... end -> 'end' here is the breaker of lang, but also breaks sem.
--
-- 3. Hard break: a new keyword forces us to reinterpret a previous assumption.
--    Example: let ... let ... lang
--      -> The second 'let' was initially assumed to be nested (a 'let in'),
--      but encountering 'lang' reveals that it's actually a top-level 'let' block,
--      requiring us to restructure the tree accordingly.    
--
-- The includes are handled via a hashset allowing us to know if we already visited a given include before jumping in it's code.
--
-- Result is a `DocTree` for the entire file. 


include "./lexing/lexer.mc"

include "./doc-tree.mc"

include "../global/util.mc"
include "../options/options.mc"
include "../execution-context.mc"

include "seq.mc"
include "hashmap.mc"
include "fileutils.mc"
include "hashmap.mc"
include "sys.mc"


-- # The parse function
-- - Takes in input a miking program
-- - And the name of the output root
-- - Returns the corresponding `DocTree`.
-- - Assume that the entry is a valid Miking program.
let parse : (ExecutionContext ->  ExecutionContext) = use TokenReader in use BreakerChooser in lam execCtx.

    let basePath: String = execCtx.mainFile in
    
    -- Keywords that start new blocks (head snippets)
    -- Using HashSet to improve performances
    let headSnippets =
        foldl
        (lam m. lam k. hmInsert k () m)
            (hashmapEmpty ())
        ["let", "lang", "type", "syn", "sem", "con", "mexpr", "use", "utest", "recursive"] in

    -- Extra breakers (manually added).
    -- We would like to ignore `switch` keyword, but we cant becauses it ends with then end keyword. It may break a lang block in some situations.
    let breakerAdder = [("switch", ["end"]), ("match", ["then", "in"]), ("if", ["then"])] in

    -- Snippet type = partial parse result
    type Snippet = {
        pos: Pos,
        tree: [DocTree],
        stream: TokenStream,
        breaker: String,
        toAdd: [DocTree],
        absorbed: Bool
    } in

    type ParseRes = {
        includeSet: IncludeSet (),
        lexingCtx: LexingCtx,
        tree: [DocTree]
    } in
    
    let parseRes2tree : ParseRes -> String -> DocTree = lam parseRes. lam progName.
        Node {
            sons = parseRes.tree,
            pos = { x = 1, y = 1 },
            token = ProgramToken {
                content = progName,
                includeSet = parseRes.includeSet
            },
            state = Program {}
        }
    in
    
    -- Access top of breaker stack
    let topState = lam breakers. let h = (head breakers).0 in h.state in
    let topBreakers = lam breakers. let h = (head breakers).0 in h.breakers in
    let baseBreaker = [({ breakers = [""], state = Program {} }, false)] in
    
    recursive
    let parseStream: TokenStream -> Pos -> [(Breaker, Bool)] -> [DocTree] -> Snippet =
        lam oldStream. lam oldPos. lam breakers. lam treeAcc.

            -- Main recursion:
            -- stream = input string
            -- breakers = stack of current block contexts
            -- treeAcc = accumulated tree so far
            --
            -- Builds a snippet when a head snippet token is encountered
            let buildSnippet : (Token -> TokenStream -> Pos -> [(Breaker, Bool)] -> [DocTree] -> Snippet) = lam token. lam stream. lam pos. lam breakers. lam treeAcc.
                let lword = content token in
                let oldState = topState breakers in
                let breakers = cons ((choose (oldState, lword, pos)), false) breakers in
                let newState = topState breakers in
    
                -- Parse the snippet content
                let isTop = eqi 2 (length breakers) in
                let snippet = parseStream stream pos breakers [] in
                let reStructureTest = reStructureTree (newState, snippet.breaker) in
                let continueTest = or isTop (continue (newState, snippet.breaker)) in
                let newState = switchVersion (newState, snippet.breaker) in
    
                -- Handle continue case (normal exit)
                if continueTest then
                    match (match snippet.stream with [(last, lastPos)] ++ stream then (last, lastPos, stream) else (Eof {}, pos, [])) with (last, lastPos, lastStream) in
    
                    match (if and (not snippet.absorbed) (absorbIt (newState, content last)) then
                        let leaf = Leaf { token = last, state = newState, pos = lastPos } in
                        { stream = lastStream, newPos = lastPos, sons = concat snippet.tree [leaf] }
                    else
                        { stream = snippet.stream, newPos = snippet.pos, sons = snippet.tree }) with
                    { stream = stream, sons = sons, newPos = newPos } in

                    let docNode = Node { sons = sons, pos = pos, token = token, state = newState } in
                    let tree = reverse (cons docNode snippet.toAdd) in
                    parseStream stream newPos (tail breakers) (concat tree treeAcc)
                else
                    -- Handle hard break
                    let docNode = Node {
                        pos = pos,
                        sons = snippet.tree, token = token,
                        state = newState
                    } in
                    let concatToAdd = cons docNode snippet.toAdd in
                    {
                        snippet with
                        toAdd = if reStructureTest then concatToAdd else [],
                        tree = if reStructureTest then reverse treeAcc else reverse (concat (reverse concatToAdd) treeAcc)
                    }
            in

            switch oldStream 
            case [(Eof {}, pos)] then
                { tree = reverse treeAcc, pos = pos, stream = oldStream, breaker = "", toAdd = [], absorbed = true }
            case [(token, pos)] ++ stream then

            let lword = content token in
            let state = topState breakers in
        
            if contains (topBreakers breakers) lword then
                if (head breakers).1 then
                    let acc = (cons (Leaf { token = token, state = state, pos = pos }) treeAcc) in
                    parseStream stream pos (tail breakers) acc
                else
                    let absorb = absorbIt (state, lword) in
                    {
                        tree = reverse (if absorb then
                                            cons (Leaf { token = token, state = state, pos = pos }) treeAcc
                                        else treeAcc),
                        stream = if absorb then stream else oldStream,
                        absorbed = absorb,
                        pos = if absorb then pos else oldPos,
                        breaker = lword, toAdd = []
                    }
            else match find (lam w. eqString w.0 lword) breakerAdder with Some b then
                -- Extra breakers (example: switch/end)
                parseStream
                    stream
                    pos
                    (cons ( { breakers = b.1, state = state }, true ) breakers)
                    (cons (Leaf { token = token, state = state, pos = pos }) treeAcc)
            else if hmMem lword headSnippets then
                -- If head snippet -> build new snippet block
                buildSnippet token stream pos breakers treeAcc
            else
                -- Default case: accumulate leaf
                parseStream stream pos breakers (cons (Leaf { token = token, state = state, pos = pos }) treeAcc)
            end
    
    let parse: IncludeSet () -> String -> LexingCtx -> ParseRes = lam includeSet. lam loc. lam lexingCtx.
        
        match parsingOpenFile loc with { includes = includes, headerTokens = headerTokens, fileText = fileText } in
        
        let headerDocTree = foldl (lam arg: ParseRes. lam token.
            match arg with { lexingCtx = lexingCtx, includeSet = includeSet, tree = tree } in
            match token with { token = token, pos = pos } in

            let go : ParseRes -> DocTree -> ParseRes = lam arg. lam doctree. { arg with tree = cons doctree arg.tree } in
            match token with Include { content = content } then
                match includeSetInsert includeSet loc content () with
                { includeSet = includeSet, inserted = inserted, path = path, isStdlib = isStdlib } in
                let insertResult = if inserted then
                    match parse includeSet path lexingCtx with
                    { includeSet = includeSet, lexingCtx = lexingCtx } & parseRes in
                    ({ arg with includeSet = includeSet, lexingCtx = lexingCtx }, Some (parseRes2tree parseRes path))
                else
                    (arg, None {}) in
                match insertResult with (arg, tree) in
                go arg (IncludeNode { token = token, tree = tree, state = Program {}, path = path, isStdlib = isStdlib, pos = pos })
            else
                go arg (Leaf { token = token, state = Program {}, pos = pos })
            ) { includeSet = includeSet, lexingCtx = lexingCtx, tree = [] } headerTokens
        in
        match headerDocTree with { includeSet = includeSet, tree = headerTree, lexingCtx = lexingCtx } in
        parsingLog (concat "Beggining of parsing stage on " loc);
        match lex lexingCtx fileText with { stream = stream, ctx = lexingCtx } in

        let snippet = parseStream stream { x = 1, y = 1 } baseBreaker headerTree in
        { includeSet = includeSet, lexingCtx = lexingCtx, tree = snippet.tree } 
    in
    
    match execCtx with { ast = Some ast } in

    let lexingCtx = lexingCtxNew ast in

    match goHere (sysGetCwd()) basePath with { path = basePos } in

    let includeSet = includeSetNew (dirname basePos) in

    match includeSetInsert includeSet "." basePath () with { includeSet = includeSet } in    

    match parse includeSet basePath lexingCtx with { includeSet = includeSet, tree = tree } & parseRes in
    parsingLog (join ["Parsing is over, computed prefix: ", includeSetPrefix includeSet, "."]);
    let tree = parseRes2tree parseRes basePath in
    displayTree tree;
    execContextWithTree execCtx tree

