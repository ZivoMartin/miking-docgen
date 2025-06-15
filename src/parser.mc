include "doc-tree.mc"
include "util.mc"

let parse = use TokenReader in use BreakerChooser in lam fileName.
 let headSnippets = ["let", "lang", "type", "syn", "sem", "con", "mexpr", "use"] in
 type Snippet = { tree: [DocTree], stream: String, breaker: String, toAdd: [DocTree] } in
    
 recursive   
 let parseRec: ([Char] -> [Breaker] -> [DocTree] -> Snippet) =
    lam stream. lam breakers. lam treeAcc.

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
        let buildSnippet : (NextResult -> [Breaker] -> [DocTree] -> Snippet) = lam word. lam breakers. lam treeAcc.
            let lword = lit word.token in
            let oldState = (head breakers).state in
            let breakers = cons (choose (oldState, lword)) breakers in
            let newState = (head breakers).state in
            let snippet = parseRec word.stream breakers [] in
            if continue (newState, snippet.breaker) then
                let docNode = Node { sons = snippet.tree, token = word.token, state = newState } in
                let tree = reverse (cons docNode snippet.toAdd) in
                parseRec snippet.stream (tail breakers) (concat tree treeAcc)
            else
                
                let docNode = Node {
                    sons = snippet.tree, token = word.token,
                    state = topVersion newState } in
                let concatToAdd = cons docNode snippet.toAdd in
                let rev = reverse treeAcc in
                let isHardTest = isHard (newState, snippet.breaker) in
                {
                    snippet with
                    toAdd = if isHardTest then concatToAdd else [],
                    tree = if isHardTest then rev else reverse (concat concatToAdd rev)
                }
        in


        let word = next stream in
        let lword = lit word.token in

        let state = (head breakers).state in
        if contains (head breakers).breakers lword then
            let absorb = absorbIt (state, lword) in
            {
                tree = reverse (if absorb then cons (Leaf (word.token, state)) treeAcc else treeAcc),
                stream = if absorb then word.stream else stream,
                breaker = lword, toAdd = [] }
        else match word.token with Eof {} then
            { tree = reverse (cons (Leaf (word.token, state)) treeAcc), stream = "", breaker = "", toAdd = [] }
        else if contains headSnippets lword then
            buildSnippet word breakers treeAcc
        else
            parseRec word.stream breakers (cons (Leaf (word.token, state)) treeAcc)
        in
    let snippet = parseRec "stream" [ { breakers = [""], state = Program {} }] [] in
    Node { sons = snippet.tree, token = Word { content = fileName }, state = Program {} }
