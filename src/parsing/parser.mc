-- # Parser: building a DocTree from a source file 
--
-- This module defines a parser that reads a file and produces a `DocTree`, a hierarchical
-- structure of tokens annotated with formatting state.
--
-- The parsing logic supports **breakers** (see breaker-choosers.mc)
-- and implements three behaviors:
--
-- When a break occurs, there are three possible behaviors:
-- 1. Local break: we simply finish the current block.
--    Example: lang ... end -> 'end' only terminates the 'lang' block.
--
-- 2. Global break: we terminate not just the current block, but also one or more parent blocks.
--    Example: lang ... sem ... end -> 'end' closes both 'sem' and 'lang'.
--
-- 3. Hard break: a new keyword forces us to reinterpret a previous assumption.
--    Example: let ... let ... lang
--      -> The second 'let' was initially assumed to be nested (a 'let in'),
--      but encountering 'lang' reveals that it's actually a top-level 'let' block,
--      requiring us to restructure the tree accordingly.    
--
-- Result is a `DocTree` for the entire file. 

    
include "doc-tree.mc"
include "../util.mc"
include "ext/file-ext.mc"
include "seq.mc"
include "hashmap.mc"
    
-- # The parse function
-- - Takes in input a String representing a Miking program.
-- - Returns the corresponding `DocTree`.
-- - Assume that the entry is a valid Miking program.
let parse : (String -> Option DocTree) = use TokenReader in use BreakerChooser in lam fileName.
    -- Keywords that start new blocks (head snippets)
    -- Using HashSet to improve performances
    let headSnippets =
        foldl
        (lam m. lam k. hmInsert k () m)
        (hashmapEmpty ())
        ["let", "lang", "type", "syn", "sem", "con", "mexpr", "use", "utest"] in

    -- Extra breakers (manually added).
    -- We would like to ignore `switch` keyword, but we cant becauses it ends with then end keyword. It may break a lang block in some situations.
    let breakerAdder = [("switch", ["end"]), ("match", ["then", "in"])] in

    -- Snippet type = partial parse result
    type Snippet = { tree: [DocTree], stream: String, breaker: String, toAdd: [DocTree], absorbed: Bool } in

    -- Access top of breaker stack
    let topState = lam breakers. let h = (head breakers).0 in h.state in
    let topBreakers = lam breakers. let h = (head breakers).0 in h.breakers in

    recursive
    let parseRec: ([Char] -> [(Breaker, Bool)] -> [DocTree] -> Snippet) =
        lam stream. lam breakers. lam treeAcc.
            -- Main recursion:
            -- stream = input string
            -- breakers = stack of current block contexts
            -- treeAcc = accumulated tree so far
            --
            -- Builds a snippet when a head snippet token is encountered
            let buildSnippet : (NextResult -> [(Breaker, Bool)] -> [DocTree] -> Snippet) = lam word. lam breakers. lam treeAcc.
                let lword = lit word.token in
                let oldState = topState breakers in
                let breakers = cons ((choose (oldState, lword)), false) breakers in
                let newState = topState breakers in

                -- Parse the snippet content
                let snippet = parseRec word.stream breakers [] in

                -- Handle continue case (normal exit)
                if continue (newState, snippet.breaker) then
                    let last = next snippet.stream in
                    let result = if and (not snippet.absorbed) (absorbIt (newState, lit last.token)) then
                        let leaf = Leaf { token = last.token, state = newState } in
                        let docNode = Node { sons = (concat snippet.tree [leaf]), token = word.token, state = newState } in
                        { stream = last.stream, docNode = docNode }
                    else
                        let docNode = Node { sons = snippet.tree, token = word.token, state = newState } in
                        { stream = snippet.stream, docNode = docNode } in

                    let tree = reverse (cons result.docNode snippet.toAdd) in
                    parseRec result.stream (tail breakers) (concat tree treeAcc)
                else
                    -- Handle hard break
                    let docNode = Node {
                        sons = snippet.tree, token = word.token,
                        state = topVersion newState } in
                    let concatToAdd = cons docNode snippet.toAdd in
                    let isHardTest = isHard (newState, snippet.breaker) in
                    {
                        snippet with
                        toAdd = if isHardTest then concatToAdd else [],
                        tree = if isHardTest then
                                reverse treeAcc
                               else
                                reverse (concat concatToAdd treeAcc)
                    }
            in

            -- Next token
            let word = next stream in
            let lword = lit word.token in
            let state = topState breakers in

            -- If current token is a breaker
            if contains (topBreakers breakers) lword then
                if (head breakers).1 then
                    parseRec word.stream (tail breakers) (cons (Leaf { token = word.token, state = state }) treeAcc)
                else
                    let absorb = absorbIt (state, lword) in
                    {
                        tree = reverse (if absorb then
                                            cons (Leaf { token = word.token, state = state }) treeAcc
                                        else treeAcc),
                        stream = if absorb then word.stream else stream,
                        absorbed = absorb,
                        breaker = lword, toAdd = [] }
            else match word.token with Eof {} then
                -- End of file: close everything
                { tree = reverse (cons (Leaf { token = word.token, state = state }) treeAcc), stream = "", breaker = "", toAdd = [], absorbed = true }
            else match find (lam w. eqString w.0 lword) breakerAdder with Some b then
                -- Extra breakers (example: switch/end)
                parseRec
                    word.stream
                    (cons ( { breakers = b.1, state = state }, true ) breakers)
                    (cons (Leaf { token = word.token, state = state }) treeAcc)
            else if hmMem lword headSnippets then
                -- If head snippet -> build new snippet block
                buildSnippet word breakers treeAcc
            else
                -- Default case: accumulate leaf
                parseRec word.stream breakers (cons (Leaf { token = word.token, state = state }) treeAcc)

    in
    -- Main entry point: open file, start recursive parsing
    match fileReadOpen fileName with Some rc then
        let s = fileReadString rc in
        fileReadClose rc;
        let snippet = parseRec s [({ breakers = [""], state = Program {} }, false)] [] in
        Some (Node { sons = snippet.tree, token = Word { content = fileName }, state = Program {} })
    else
        None {}
