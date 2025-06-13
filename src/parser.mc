include "token-readers.mc"
include "string.mc"
include "util.mc"

lang BreakerChooserInterface

    type Breaker = { breakers: [String], state: State }

    
    sem chooseCrash /- (State, String) -> () -/ =
        | (state, word) -> error
                (concatAll ["You cannot have the word ", word, " inside a ", (toString state), " block."])
    sem topVersionCrash /- (State) -> () -/ =
        | state -> error (concat (toString state) " does not have any top version")

        
    syn State =
        | Program {}
        | TopLet {}
        | Let {}
        | Lang {}
        | TopType {}
        | Type {}

    sem toString =
        | Program {} -> "Program"
        | TopLet {} -> "TopLet"
        | Let {} -> "Let"
        | Lang {} -> "Lang"
        | TopType {} -> "TopType"
        | Type {} -> "Type"

    -- Determine the new state and the breakers after having find a block opener
    sem choose /- (State, String) -> Breaker -/ =
    -- Determine if for a given breaker, the tokenisation should continue for the parent state
    sem continue /- (State, String) -> Bool -/ =
    -- Determine for a given context if the block become hard or no
    sem isHard /- (State, String) -> Bool -/ =
    -- Determine if the breaker should be part of the current block, or should be let in the stream
    sem absorbIt /- (State, String) -> Bool -/ =
    -- TopVersion if possible the given state to it's top version, crash otherwise.
    sem topVersion /- State -> State -/ = 
end

-- TODO: let, lang, con, type, use, sem, syn
-- TODO: Suppporter les mots clés qui arrêtent plusieurs étages à la fois comme end

lang ProgramBreakerChooser = BreakerChooserInterface

    sem choose =
        | (Program {}, "let") -> { breakers = ["lang", "in"], state = TopLet {} }
        | (Program {}, "lang") -> { breakers = ["end"], state = Lang {} }
        | (Program {}, "type") -> { breakers = ["let", "type", "lang"], state = TopType {} }
        | (Program {}, word) -> chooseCrash (Program {}, word)

    sem continue =
        | (Program {}, "") -> false
        | (Program {}, _) -> true

    sem absorbIt =
        | (Program {}, word) -> true

    sem topVersion =
        | Program {} -> topVersionCrash (Program {})
end

lang TopLetBreakerChooser = BreakerChooserInterface
    
    sem choose =
        | (TopLet {}, "let") -> { breakers = ["in", "lang"], state = Let {} }
        | (TopLet {}, word) -> chooseCrash (TopLet {}, word)

    sem continue =
        | (TopLet {}, _) -> true

    sem isHard =
        | (TopLet {}, _) -> false

    sem absorbIt =
        | (TopLet {}, word) -> false

    sem topVersion =
        | TopLet {} -> topVersionCrash (TopLet {})
end
    
lang LetBreakerChooser = BreakerChooserInterface

    sem choose =
        | (Let {}, "let") -> { breakers = ["in", "lang"], state = Let {} }
        | (Let {}, "type") -> error "todo: Support type in let blocks"
        | (Let {}, word) -> chooseCrash (Let {}, word)

    sem continue =
        | (Let {}, "in") -> true
        | (Let {}, "lang") -> false

    sem isHard =
        | (Let {}, _) -> true
        | (Let {}, "in") -> error "Unreachable, we should not ask the state machine if finding a 'in' bound to a 'let' makes the block hard."
    
    sem absorbIt =
        | (Let {}, "in") -> true
        | (Let {}, word) -> false

    sem topVersion =
        | Let {} -> TopLet {}

end
    
lang LangBreakerChooser = BreakerChooserInterface

    sem choose =
        | (Lang {}, word) -> chooseCrash (Lang {}, word)

    sem absorbIt =
        | (Lang {}, word) -> true

    sem continue =
        | (Lang {}, word) -> true

    sem topVersion =
        | Lang {} -> topVersionCrash (Lang {})

end



lang BreakerChooser = ProgramBreakerChooser + TopLetBreakerChooser + LetBreakerChooser + LangBreakerChooser end

type DocTree
con Node : use TokenReader in use BreakerChooser in { sons: [DocTree], token: Token, state: State} -> DocTree
con Leaf : use TokenReader in use BreakerChooser in (Token, State) -> DocTree

    
    
let displayTree : (DocTree -> ()) = use TokenReader in use BreakerChooser in lam tree.
  recursive let replicate = lam n. lam str.
        if eqi n 0 then [] else cons str (replicate (subi n 1) str) in
    
  let indentString = lam n.
    if eqi n 0 then "" else
      concatAll (replicate n "  ")
  in
  recursive let displayTreeIndented = lam tree. lam depth.
    match tree with Node { sons = sons, token = token, state = state } then
         print (concatAll [indentString depth, "Node (", toString state, "): ", lit token, "\n"]);
        iter (lam child. displayTreeIndented child (addi depth 1)) sons
    else match tree with Leaf (token, state) then
        match token with Separator {} | Eof {} then () else 
            print (concatAll [indentString depth, "Leaf: ", lit token, "\n"])
    else never
  in
  displayTreeIndented tree 0

let parse = use TokenReader in use BreakerChooser in lam stream.
 let headSnippets = ["let", "lang"] in

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
                let isHardTest = isHard (newState, snippet.breaker) in
                {
                    snippet with
                    toAdd = if isHardTest then cons docNode snippet.toAdd else snippet.toAdd,
                    tree = if isHardTest then [] else cons docNode snippet.tree
                }
        in

        recursive
        let contains = lam arr. lam lword. 
            match arr with [] then
                false
            else
                or (eqString (head arr) lword) (contains (tail arr) lword)
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
