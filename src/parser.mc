include "token-readers.mc"
include "breaker-choosers.mc"
include "doc-tree.mc"
include "util.mc"

let parse = use TokenReader in use BreakerChooser in lam stream.
 let headSnippets = ["let", "lang", "type", "syn", "sem", "con", "mexpr"] in

 type Snippet = { tree: [DocTree], stream: [Char], breaker: String, toAdd: [DocTree] } in
    
 recursive   
 let parseRec: ([Char] -> [Breaker] -> Snippet) =
    lam stream. lam breakers.

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
        let buildSnippet : (NextResult -> [Breaker] -> Snippet) = lam word. lam breakers.
            let lword = lit word.token in
            let oldState = (head breakers).state in
            let breakers = cons (choose (oldState, lword)) breakers in
            let newState = (head breakers).state in
            let snippet = parseRec word.stream breakers in
            if continue (newState, snippet.breaker) then
                let docNode = Node { sons = snippet.tree, token = word.token, state = newState } in
                let tree = cons docNode snippet.toAdd in
                let output = parseRec snippet.stream (tail breakers) in
                { output with tree = concat tree output.tree }
            else
                
                let docNode = Node {
                    sons = snippet.tree, token = word.token,
                    state = topVersion newState } in
                let concatToAdd = cons docNode snippet.toAdd in
                let isHardTest = isHard (newState, snippet.breaker) in
                {
                    snippet with
                    toAdd = if isHardTest then concatToAdd else [],
                    tree = if isHardTest then [] else concatToAdd
                }
        in


        let word = next stream in
        let lword = lit word.token in

        let state = (head breakers).state in
        if contains (head breakers).breakers lword then
            let absorb = absorbIt (state, lword) in
            {
                tree = if absorb then  [Leaf (word.token, state)] else [],
                stream = if absorb then word.stream else stream,
                breaker = lword, toAdd = [] }
        else match word.token with Eof {} then
            { tree = [Leaf (word.token, state)], stream = "", breaker = "", toAdd = [] }
        else if contains headSnippets lword then
            buildSnippet word breakers
        else
            let snippet = parseRec word.stream breakers in
            { snippet with tree = cons (Leaf (word.token, state)) snippet.tree }
        in
    let snippet = parseRec stream [ { breakers = [""], state = Program {} }] in
    Node { sons = snippet.tree, token = Word { content = "" }, state = Program {} }
