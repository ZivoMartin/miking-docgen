include "doc-tree.mc"
include "util.mc"
include "ext/file-ext.mc"
include "seq.mc"

let parse : (String -> Option DocTree) = use TokenReader in use BreakerChooser in lam fileName.
    let headSnippets = ["let", "lang", "type", "syn", "sem", "con", "mexpr", "use"] in
    let breakerAdder = [("switch", ["end"])] in
    type Snippet = { tree: [DocTree], stream: String, breaker: String, toAdd: [DocTree], absorbed: Bool } in
    let topState = lam breakers. let h = (head breakers).0 in h.state in
    let topBreakers = lam breakers. let h = (head breakers).0 in h.breakers in
    
    
        
    recursive   
    let parseRec: ([Char] -> [(Breaker, Bool)] -> [DocTree] -> Snippet) =
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
            let buildSnippet : (NextResult -> [(Breaker, Bool)] -> [DocTree] -> Snippet) = lam word. lam breakers. lam treeAcc.
                let lword = lit word.token in
                let oldState =  topState breakers in
                let breakers = cons ((choose (oldState, lword)), false) breakers in
                let newState = topState breakers in
                let snippet = parseRec word.stream breakers [] in
                if continue (newState, snippet.breaker) then
                    let last = next snippet.stream in
                    
                    let result = if and (not snippet.absorbed) (absorbIt (newState, lit last.token)) then
                        let leaf = Leaf (last.token, newState) in
                        let docNode = Node { sons = (concat snippet.tree [leaf]), token = word.token, state = newState } in
                        { stream = last.stream, docNode = docNode }
                    else
                        let docNode = Node { sons = snippet.tree, token = word.token, state = newState } in    
                        { stream = snippet.stream, docNode = docNode } in
                    let tree = reverse (cons result.docNode snippet.toAdd) in
                    parseRec result.stream (tail breakers) (concat tree treeAcc)
                else                   
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
            let word = next stream in
            let lword = lit word.token in
            let state = topState breakers in
            if contains (topBreakers breakers) lword then
                if (head breakers).1 then
                     parseRec word.stream (tail breakers) (cons (Leaf (word.token, state)) treeAcc)
                else 
                    let absorb = absorbIt (state, lword) in
                    {
                        tree = reverse (if absorb then
                                            cons (Leaf (word.token, state)) treeAcc
                                        else treeAcc),
                        stream = if absorb then word.stream else stream,
                        absorbed = absorb,
                        breaker = lword, toAdd = [] }
            else match word.token with Eof {} then
                    { tree = reverse (cons (Leaf (word.token, state)) treeAcc), stream = "", breaker = "", toAdd = [], absorbed = true }
            else match find (lam w. eqString w.0 lword) breakerAdder with Some b then
                parseRec
                    word.stream
                    (cons ( { breakers = b.1, state = state }, true ) breakers)
                    (cons (Leaf (word.token, state)) treeAcc)
            else if contains headSnippets lword then
                buildSnippet word breakers treeAcc
            else
                parseRec word.stream breakers (cons (Leaf (word.token, state)) treeAcc)
            in
        match fileReadOpen fileName with Some rc then
            let s = fileReadString rc in
            fileReadClose rc;
            let snippet = parseRec s [({ breakers = [""], state = Program {} }, false)] [] in
            Some (Node { sons = snippet.tree, token = Word { content = fileName }, state = Program {} })
        else None {}
 
