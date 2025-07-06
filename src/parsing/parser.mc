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
-- Result is a `DocTree` for the entire file. 

    
include "doc-tree.mc"
include "../util.mc"

include "seq.mc"
include "hashmap.mc"
include "ext/file-ext.mc"
    
-- # The parse function
-- - Takes in input a path to a Miking file
-- - Returns the corresponding `DocTree`.
-- - Assume that the entry is a valid Miking program.
let parse : (String -> String -> DocTree) = use TokenReader in use BreakerChooser in lam code. lam progName.
    -- Keywords that start new blocks (head snippets)
    -- Using HashSet to improve performances
    let headSnippets =
        foldl
        (lam m. lam k. hmInsert k () m)
        (hashmapEmpty ())
        ["let", "lang", "type", "syn", "sem", "con", "mexpr", "use", "utest", "recursive"] in

    -- Extra breakers (manually added).
    -- We would like to ignore `switch` keyword, but we cant becauses it ends with then end keyword. It may break a lang block in some situations.
    let breakerAdder = [("switch", ["end"]), ("match", ["then", "in"])] in

    -- Snippet type = partial parse result
    type Snippet = { pos: Pos, tree: [DocTree], stream: String, breaker: String, toAdd: [DocTree], absorbed: Bool } in

    -- Access top of breaker stack
    let topState = lam breakers. let h = (head breakers).0 in h.state in
    let topBreakers = lam breakers. let h = (head breakers).0 in h.breakers in

    recursive
    let parseRec: [Char] -> Pos -> [(Breaker, Bool)] -> [DocTree] -> Snippet =
        lam stream. lam pos. lam breakers. lam treeAcc.
            -- Main recursion:
            -- stream = input string
            -- breakers = stack of current block contexts
            -- treeAcc = accumulated tree so far
            --
            -- Builds a snippet when a head snippet token is encountered
            let buildSnippet : (NextResult -> [(Breaker, Bool)] -> [DocTree] -> Snippet) = lam word. lam breakers. lam treeAcc.
                let lword = content word.token in
                let oldState = topState breakers in
                let breakers = cons ((choose (oldState, lword, word.pos)), false) breakers in
                let newState = topState breakers in

                -- Parse the snippet content
                let isTop = eqi 2 (length breakers) in
                let snippet = parseRec word.stream word.pos breakers [] in
                let reStructureTest = reStructureTree (newState, snippet.breaker) in
                let continueTest = or isTop (continue (newState, snippet.breaker)) in
                let newState = switchVersion (newState, snippet.breaker) in
    
                -- Handle continue case (normal exit)
                if continueTest then
                    let last = next snippet.stream snippet.pos in
                    let result = if and (not snippet.absorbed) (absorbIt (newState, content last.token)) then
                        let leaf = Leaf { token = last.token, state = newState } in
                        let docNode = Node { sons = (concat snippet.tree [leaf]), token = word.token, state = newState } in
                        { stream = last.stream, pos = last.pos, docNode = docNode }
                    else
                        let docNode = Node { sons = snippet.tree, token = word.token, state = newState } in
                        { stream = snippet.stream, pos = snippet.pos, docNode = docNode } in

                    let tree = reverse (cons result.docNode snippet.toAdd) in
                    parseRec result.stream result.pos (tail breakers) (concat tree treeAcc)
                else
                    -- Handle hard break
                    let docNode = Node {
                        sons = snippet.tree, token = word.token,
                        state = newState } in
                    let concatToAdd = cons docNode snippet.toAdd in
                    {
                        snippet with
                        toAdd = if reStructureTest then concatToAdd else [],
                        tree = if reStructureTest then
                                reverse treeAcc
                               else
                                reverse (concat concatToAdd treeAcc)
                    }
            in

            -- Next token
            let word = next stream pos in
            let lword = content word.token in
            let state = topState breakers in

            -- If current token is a breaker
            if contains (topBreakers breakers) lword then
                if (head breakers).1 then
                    parseRec word.stream word.pos (tail breakers) (cons (Leaf { token = word.token, state = state }) treeAcc)
                else
                    let absorb = absorbIt (state, lword) in
                    {
                        tree = reverse (if absorb then
                                            cons (Leaf { token = word.token, state = state }) treeAcc
                                        else treeAcc),
                        stream = if absorb then word.stream else stream,
                        absorbed = absorb,
                        pos = if absorb then word.pos else pos,
                        breaker = lword, toAdd = [] }
            else match word.token with Eof {} then
                -- End of file: close everything
                { tree = reverse treeAcc, pos = word.pos, stream = "", breaker = "", toAdd = [], absorbed = true }
            else match find (lam w. eqString w.0 lword) breakerAdder with Some b then
                -- Extra breakers (example: switch/end)
                parseRec
                    word.stream
                    word.pos
                    (cons ( { breakers = b.1, state = state }, true ) breakers)
                    (cons (Leaf { token = word.token, state = state }) treeAcc)
            else if hmMem lword headSnippets then
                -- If head snippet -> build new snippet block
                buildSnippet word breakers treeAcc
            else
                -- Default case: accumulate leaf
                parseRec word.stream word.pos breakers (cons (Leaf { token = word.token, state = state }) treeAcc)

    in
    let snippet = parseRec code { x = 0, y = 0 } [({ breakers = [""], state = Program {} }, false)] [] in
    parsingLog "Parsing is over.";
    Node { sons = snippet.tree, token = ProgramToken { content = progName }, state = Program {} }

let parseFile : (String -> Option DocTree) = lam fileName.
    parsingLog (concat "Beggining of parsing stage on " fileName);
    match fileReadOpen fileName with Some rc then
        let s = fileReadString rc in
        fileReadClose rc;
        Some (parse s fileName)
    else
        None {}



mexpr use BreakerChooser in

-- 1. Basic single block parsing
utest parse "let x = 1" "" with 
  (Node { token = ProgramToken { content = "" }, state = Program {}, sons =
    [Node { token = Word { content = "let" }, state = TopLet {}, sons = [
        Leaf { token = Separator { content = " " }, state = TopLet {} },
        Leaf { token = Word { content = "x" }, state = TopLet {} },
        Leaf { token = Separator { content = " " }, state = TopLet {} },    
        Leaf { token = Word { content = "=" }, state = TopLet {} },
        Leaf { token = Separator { content = " " }, state = TopLet {} },    
        Leaf { token = Word { content = "1" }, state = TopLet {} }]},
    Leaf { token = Eof {}, state = Program {}}]
  }) in

utest parse "lang end" "" with 
  (Node { token = ProgramToken { content = "" }, state = Program {}, sons =
    [Node { token = Word { content = "lang" }, state = Lang {}, sons = [
        Leaf { token = Separator { content = " " }, state = Lang {} },
        Leaf { token = Word { content = "end" }, state = Lang {} }
    ]},
    Leaf { token = Eof {}, state = Program {}}]}) in


-- 2. Nested snippet structure
utest parse "let let in " "" with 
  (Node { token = ProgramToken { content = "" }, state = Program {}, sons = [
    Node { token = Word { content = "let" }, state = TopLet {}, sons = [
        Leaf { token = Separator { content = " " }, state = TopLet {} },
        Node { token = Word { content = "let" }, state = Let {}, sons = [
            Leaf { token = Separator { content = " " }, state = Let {} },
            Leaf { token = Word { content = "in" }, state = Let {} }
        ]},
        Leaf { token = Separator { content = " " }, state = TopLet {} }
    ]},
    Leaf { token = Eof {}, state = Program {}}
  ]}) in


-- 3. Absorbed vs non-absorbed breakers
utest parse "mexpr let x = 1 in" "" with 
  (Node { token = ProgramToken { content = "" }, state = Program {}, sons = [
    Node { token = Word { content = "mexpr" }, state = Mexpr {}, sons = [
      Leaf { token = Separator { content = " " }, state = Mexpr {} },        
      Node { token = Word { content = "let" }, state = Let {}, sons = [
        Leaf { token = Separator { content = " " }, state = Let {} },    
        Leaf { token = Word { content = "x" }, state = Let {} },
        Leaf { token = Separator { content = " " }, state = Let {} },    
        Leaf { token = Word { content = "=" }, state = Let {} },
        Leaf { token = Separator { content = " " }, state = Let {} },
        Leaf { token = Word { content = "1" }, state = Let {} },
        Leaf { token = Separator { content = " " }, state = Let {} },    
        Leaf { token = Word { content = "in" }, state = Let {} }
      ]}
    ]},
    Leaf { token = Eof {}, state = Program {}}
  ]}) in


-- 4. Hard break handling
utest parse "let x = 3 let y = 2 lang end" "" with 
(Node { token = ProgramToken { content = "" }, state = Program {}, sons = [
    Node { token = Word { content = "let" }, state = TopLet {}, sons = [
        Leaf { token = Separator { content = " " }, state = TopLet {} },    
        Leaf { token = Word { content = "x" }, state = TopLet {} },
        Leaf { token = Separator { content = " " }, state = TopLet {} },
        Leaf { token = Word { content = "=" }, state = TopLet {} },
        Leaf { token = Separator { content = " " }, state = TopLet {} },
        Leaf { token = Word { content = "3" }, state = TopLet {} },
        Leaf { token = Separator { content = " " }, state = TopLet {} }
    ]},
    Node { token = Word { content = "let" }, state = TopLet {}, sons = [
        Leaf { token = Separator { content = " " }, state = Let {} },    
        Leaf { token = Word { content = "y" }, state = Let {} },
        Leaf { token = Separator { content = " " }, state = Let {} },    
        Leaf { token = Word { content = "=" }, state = Let {} },
        Leaf { token = Separator { content = " " }, state = Let {} },
        Leaf { token = Word { content = "2" }, state = Let {} },
        Leaf { token = Separator { content = " " }, state = Let {} }
    ]},
    Node { token = Word { content = "lang" }, state = Lang {}, sons = [
        Leaf { token = Separator { content = " " }, state = Lang {} },    
        Leaf { token = Word { content = "end" }, state = Lang {} }
    ]},
    Leaf { token = Eof {}, state = Program {}}
]}) in



-- 5. Extra breakers
utest parse "lang switch x then end end" "" with 
  (Node { token = ProgramToken { content = "" }, state = Program {}, sons = [
    Node { token = Word { content = "lang" }, state = Lang {}, sons = [
        Leaf { token = Separator { content = " " }, state = Lang {} },
        Leaf { token = Word { content = "switch" }, state = Lang {} },
        Leaf { token = Separator { content = " " }, state = Lang {} },    
        Leaf { token = Word { content = "x" }, state = Lang {} },
        Leaf { token = Separator { content = " " }, state = Lang {} },    
        Leaf { token = Word { content = "then" }, state = Lang {} },
        Leaf { token = Separator { content = " " }, state = Lang {} },    
        Leaf { token = Word { content = "end" }, state = Lang {} },
        Leaf { token = Separator { content = " " }, state = Lang {} },    
        Leaf { token = Word { content = "end" }, state = Lang {} }
    ]},
    Leaf { token = Eof {}, state = Program {}}
  ]}) in


-- 6. Deep nesting
utest parse "let x = 2 lang sem let y = match 1 with 1 then 2 in 3 end" "" with 
(Node { token = ProgramToken { content = "" }, state = Program {}, sons = [
    Node { token = Word { content = "let" }, state = TopLet {}, sons = [
        Leaf { token = Separator { content = " " }, state = TopLet {} },
        Leaf { token = Word { content = "x" }, state = TopLet {} },
        Leaf { token = Separator { content = " " }, state = TopLet {} },    
        Leaf { token = Word { content = "=" }, state = TopLet {} },
        Leaf { token = Separator { content = " " }, state = TopLet {} },    
        Leaf { token = Word { content = "2" }, state = TopLet {} },
        Leaf { token = Separator { content = " " }, state = TopLet {} }    
    ]},
    Node { token = Word { content = "lang" }, state = Lang {}, sons = [
        Leaf { token = Separator { content = " " }, state = Lang {} },    
        Node { token = Word { content = "sem" }, state = Sem {}, sons = [
            Leaf { token = Separator { content = " " }, state = Sem {} },
            Node { token = Word { content = "let" }, state = Let {}, sons = [
                Leaf { token = Separator { content = " " }, state = Let {} },
                Leaf { token = Word { content = "y" }, state = Let {} },
                Leaf { token = Separator { content = " " }, state = Let {} },    
                Leaf { token = Word { content = "=" }, state = Let {} },
                Leaf { token = Separator { content = " " }, state = Let {} },    
                Leaf { token = Word { content = "match" }, state = Let {} },
                Leaf { token = Separator { content = " " }, state = Let {} },
                Leaf { token = Word { content = "1" }, state = Let {} },
                Leaf { token = Separator { content = " " }, state = Let {} },
                Leaf { token = Word { content = "with" }, state = Let {} },
                Leaf { token = Separator { content = " " }, state = Let {} },
                Leaf { token = Word { content = "1" }, state = Let {} },
                Leaf { token = Separator { content = " " }, state = Let {} },    
                Leaf { token = Word { content = "then" }, state = Let {} },
                Leaf { token = Separator { content = " " }, state = Let {} },
                Leaf { token = Word { content = "2" }, state = Let {} },
                Leaf { token = Separator { content = " " }, state = Let {} },    
                Leaf { token = Word { content = "in" }, state = Let {} }
          ]},
          Leaf { token = Separator { content = " " }, state = Sem {} },
          Leaf { token = Word { content = "3" }, state = Sem {} },
          Leaf { token = Separator { content = " " }, state = Sem {} }
        ]},
        Leaf { token = Word { content = "end" }, state = Lang {} }
    ]},
    Leaf { token = Eof {}, state = Program {}}
]}) in

-- 7. Recursive let
utest parse "let x = 1 recursive let y = recursive let z = 2 in 2 end" "" with 
  (Node { token = ProgramToken { content = "" }, state = Program {}, sons =
    [Node { token = Word { content = "let" }, state = TopLet {}, sons = [
        Leaf { token = Separator { content = " " }, state = TopLet {} },
        Leaf { token = Word { content = "x" }, state = TopLet {} },
        Leaf { token = Separator { content = " " }, state = TopLet {} },    
        Leaf { token = Word { content = "=" }, state = TopLet {} },
        Leaf { token = Separator { content = " " }, state = TopLet {} },    
        Leaf { token = Word { content = "1" }, state = TopLet {} },
        Leaf { token = Separator { content = " " }, state = TopLet {} }    
    ]},
    Node { token = Recursive { lit = "recursive let", skiped = [Separator { content = " " }] }, state = TopRec {}, sons = [
        Leaf { token = Separator { content = " " }, state = TopRec {} },
        Leaf { token = Word { content = "y" }, state = TopRec {} },
        Leaf { token = Separator { content = " " }, state = TopRec {} },    
        Leaf { token = Word { content = "=" }, state = TopRec {} },
        Leaf { token = Separator { content = " " }, state = TopRec {} },        
        Node { token = Recursive { lit = "recursive let", skiped = [Separator { content = " " }] }, state = Rec {}, sons = [
            Leaf { token = Separator { content = " " }, state = Rec {} },
            Leaf { token = Word { content = "z" }, state = Rec {} },
            Leaf { token = Separator { content = " " }, state = Rec {} },    
            Leaf { token = Word { content = "=" }, state = Rec {} },
            Leaf { token = Separator { content = " " }, state = Rec {} },
            Leaf { token = Word { content = "2" }, state = Rec {} },
            Leaf { token = Separator { content = " " }, state = Rec {} },
            Leaf { token = Word { content = "in" }, state = Rec {} }
        ]},
        Leaf { token = Separator { content = " " }, state = TopRec {} },    
        Leaf { token = Word { content = "2" }, state = TopRec {} },
        Leaf { token = Separator { content = " " }, state = TopRec {} },
        Leaf { token = Word { content = "end" }, state = TopRec {} }    
    ]},
    Leaf { token = Eof {}, state = Program {}}]
  }) in

    
()
