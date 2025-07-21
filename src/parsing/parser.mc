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


include "./lexer.mc"
include "./doc-tree.mc"
include "../global/util.mc"
include "../options/options.mc"

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
let parse : (String -> String -> DocTree) = use TokenReader in use BreakerChooser in lam code. lam basePath.
    
    let logBegin = lam loc. parsingLog (concat "Beggining of parsing stage on " loc) in
    logBegin basePath;

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
    type Snippet = { pos: Pos, tree: [DocTree], stream: TokenStream, breaker: String, toAdd: [DocTree], absorbed: Bool, includeSet: IncludeSet } in
    let snippet2tree : Snippet -> String -> DocTree =
    lam snippet. lam progName. Node { sons = snippet.tree, pos = { x = 1, y = 1 }, token = ProgramToken { content = progName, includeSet = snippet.includeSet }, state = Program {} } in
    
    -- Access top of breaker stack
    let topState = lam breakers. let h = (head breakers).0 in h.state in
    let topBreakers = lam breakers. let h = (head breakers).0 in h.breakers in
    let baseBreaker = [({ breakers = [""], state = Program {} }, false)] in
    
    recursive
    let parseRec: IncludeSet -> String -> TokenStream -> Pos -> [(Breaker, Bool)] -> [DocTree] -> Snippet =
        lam includeSet. lam loc. lam oldStream. lam oldPos. lam breakers. lam treeAcc.
            let parseAgain = parseRec includeSet loc in

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
                let snippet = parseRec includeSet loc stream pos breakers [] in
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
                        { stream = snippet.stream, newPos = snippet.pos, sons = snippet.tree }) with { stream = stream, sons = sons, newPos = newPos } in

                    let docNode = Node { sons = sons, pos = pos, token = token, state = newState } in
                    let tree = reverse (cons docNode snippet.toAdd) in
                    parseRec snippet.includeSet loc stream newPos (tail breakers) (concat tree treeAcc)
                else
                    -- Handle hard break
                    let docNode = Node {
                        pos = pos,
                        sons = snippet.tree, token = token,
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

            switch oldStream 
            case [(Eof {}, pos)] then
                { tree = reverse treeAcc, pos = pos, stream = oldStream, breaker = "", toAdd = [], absorbed = true, includeSet = includeSet }
            case [(token, pos)] ++ stream then

            let lword = content token in
            let state = topState breakers in
        
            match token with Include { content = content } then
                match includeSetInsert includeSet loc content with
                { includeSet = includeSet, inserted = inserted, path = path, isStdlib = isStdlib } in
                match (if inserted then        
                    let s = readOrNever path in
                    logBegin path;
                    let stream = lex s in
                    let snippet = parseRec includeSet path stream { x = 1, y = 1 } baseBreaker [] in
                    (Some (snippet2tree snippet path), snippet.includeSet)
                else (None {}, includeSet)) with (tree, includeSet) in
                let includeNode = IncludeNode { token = token, tree = tree, state = state, path = path, isStdlib = isStdlib, pos = pos } in
                parseRec includeSet loc stream pos breakers (cons includeNode treeAcc)
            -- If current token is a breaker
            else if contains (topBreakers breakers) lword then
                if (head breakers).1 then
                    let acc = (cons (Leaf { token = token, state = state, pos = pos }) treeAcc) in
                    parseAgain stream pos (tail breakers) acc
                else
                    let absorb = absorbIt (state, lword) in
                    {
                        tree = reverse (if absorb then
                                            cons (Leaf { token = token, state = state, pos = pos }) treeAcc
                                        else treeAcc),
                        stream = if absorb then stream else oldStream,
                        absorbed = absorb,
                        pos = if absorb then pos else oldPos,
                        breaker = lword, toAdd = [],
                        includeSet = includeSet
                    }
            else match find (lam w. eqString w.0 lword) breakerAdder with Some b then
                -- Extra breakers (example: switch/end)
                parseAgain
                    stream
                    pos
                    (cons ( { breakers = b.1, state = state }, true ) breakers)
                    (cons (Leaf { token = token, state = state, pos = pos }) treeAcc)
            else if hmMem lword headSnippets then
                -- If head snippet -> build new snippet block
                buildSnippet token stream pos breakers treeAcc
            else
                -- Default case: accumulate leaf
                parseAgain stream pos breakers (cons (Leaf { token = token, state = state, pos = pos }) treeAcc)
            end

    in
    let stream = lex code in
    let includeSet = includeSetNew (sysGetCwd ()) in
    match includeSetInsert includeSet "." basePath with { includeSet = includeSet } in
    let snippet = parseRec includeSet basePath stream { x = 1, y = 1 } baseBreaker [] in
    snippet2tree snippet basePath


-- Parse a Miking file, takes in argument a file, read it, and call parse with its content.
let parseFile : String -> DocTree = lam fileName.
    let s = readOrNever fileName in
    parse s fileName


mexpr use BreakerChooser in

-- 1. Basic single block parsing
utest parse "let x = 1" "" with 
  (Node {  token = ProgramToken { content = "" }, state = Program {}, sons =
    [Node {  token = Word { content = "let" }, state = TopLet {}, sons = [
        Leaf { token = Separator { content = " " }, state = TopLet {} },
        Leaf { token = Word { content = "x" }, state = TopLet {} },
        Leaf { token = Separator { content = " " }, state = TopLet {} },    
        Leaf { token = Word { content = "=" }, state = TopLet {} },
        Leaf { token = Separator { content = " " }, state = TopLet {} },    
        Leaf { token = Word { content = "1" }, state = TopLet {} }]},
    Leaf { token = Eof {}, state = Program {}}]
  }) in

utest parse "lang end" "" with 
  (Node {  token = ProgramToken { content = "" }, state = Program {}, sons =
    [Node {  token = Word { content = "lang" }, state = Lang {}, sons = [
        Leaf { token = Separator { content = " " }, state = Lang {} },
        Leaf { token = Word { content = "end" }, state = Lang {} }
    ]},
    Leaf { token = Eof {}, state = Program {}}]}) in


-- 2. Nested snippet structure
utest parse "let let in " "" with 
  (Node {  token = ProgramToken { content = "" }, state = Program {}, sons = [
    Node {  token = Word { content = "let" }, state = TopLet {}, sons = [
        Leaf { token = Separator { content = " " }, state = TopLet {} },
        Node {  token = Word { content = "let" }, state = Let {}, sons = [
            Leaf { token = Separator { content = " " }, state = Let {} },
            Leaf { token = Word { content = "in" }, state = Let {} }
        ]},
        Leaf { token = Separator { content = " " }, state = TopLet {} }
    ]},
    Leaf { token = Eof {}, state = Program {}}
  ]}) in


-- 3. Absorbed vs non-absorbed breakers
utest parse "mexpr let x = 1 in" "" with 
  (Node {  token = ProgramToken { content = "" }, state = Program {}, sons = [
    Node {  token = Word { content = "mexpr" }, state = Mexpr {}, sons = [
      Leaf { token = Separator { content = " " }, state = Mexpr {} },        
      Node {  token = Word { content = "let" }, state = Let {}, sons = [
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
(Node {  token = ProgramToken { content = "" }, state = Program {}, sons = [
    Node {  token = Word { content = "let" }, state = TopLet {}, sons = [
        Leaf { token = Separator { content = " " }, state = TopLet {} },    
        Leaf { token = Word { content = "x" }, state = TopLet {} },
        Leaf { token = Separator { content = " " }, state = TopLet {} },
        Leaf { token = Word { content = "=" }, state = TopLet {} },
        Leaf { token = Separator { content = " " }, state = TopLet {} },
        Leaf { token = Word { content = "3" }, state = TopLet {} },
        Leaf { token = Separator { content = " " }, state = TopLet {} }
    ]},
    Node {  token = Word { content = "let" }, state = TopLet {}, sons = [
        Leaf { token = Separator { content = " " }, state = Let {} },    
        Leaf { token = Word { content = "y" }, state = Let {} },
        Leaf { token = Separator { content = " " }, state = Let {} },    
        Leaf { token = Word { content = "=" }, state = Let {} },
        Leaf { token = Separator { content = " " }, state = Let {} },
        Leaf { token = Word { content = "2" }, state = Let {} },
        Leaf { token = Separator { content = " " }, state = Let {} }
    ]},
    Node {  token = Word { content = "lang" }, state = Lang {}, sons = [
        Leaf { token = Separator { content = " " }, state = Lang {} },    
        Leaf { token = Word { content = "end" }, state = Lang {} }
    ]},
    Leaf { token = Eof {}, state = Program {}}
]}) in



-- 5. Extra breakers
utest parse "lang switch x then end end" "" with 
  (Node {  token = ProgramToken { content = "" }, state = Program {}, sons = [
    Node {  token = Word { content = "lang" }, state = Lang {}, sons = [
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
(Node {  token = ProgramToken { content = "" }, state = Program {}, sons = [
    Node {  token = Word { content = "let" }, state = TopLet {}, sons = [
        Leaf { token = Separator { content = " " }, state = TopLet {} },
        Leaf { token = Word { content = "x" }, state = TopLet {} },
        Leaf { token = Separator { content = " " }, state = TopLet {} },    
        Leaf { token = Word { content = "=" }, state = TopLet {} },
        Leaf { token = Separator { content = " " }, state = TopLet {} },    
        Leaf { token = Word { content = "2" }, state = TopLet {} },
        Leaf { token = Separator { content = " " }, state = TopLet {} }    
    ]},
    Node {  token = Word { content = "lang" }, state = Lang {}, sons = [
        Leaf { token = Separator { content = " " }, state = Lang {} },    
        Node {  token = Word { content = "sem" }, state = Sem {}, sons = [
            Leaf { token = Separator { content = " " }, state = Sem {} },
            Node {  token = Word { content = "let" }, state = Let {}, sons = [
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
  (Node {  token = ProgramToken { content = "" }, state = Program {}, sons =
    [Node {  token = Word { content = "let" }, state = TopLet {}, sons = [
        Leaf { token = Separator { content = " " }, state = TopLet {} },
        Leaf { token = Word { content = "x" }, state = TopLet {} },
        Leaf { token = Separator { content = " " }, state = TopLet {} },    
        Leaf { token = Word { content = "=" }, state = TopLet {} },
        Leaf { token = Separator { content = " " }, state = TopLet {} },    
        Leaf { token = Word { content = "1" }, state = TopLet {} },
        Leaf { token = Separator { content = " " }, state = TopLet {} }    
    ]},
    Node {  token = Recursive { lit = "recursive let", skiped = [Separator { content = " " }] }, state = TopRec {}, sons = [
        Leaf { token = Separator { content = " " }, state = TopRec {} },
        Leaf { token = Word { content = "y" }, state = TopRec {} },
        Leaf { token = Separator { content = " " }, state = TopRec {} },    
        Leaf { token = Word { content = "=" }, state = TopRec {} },
        Leaf { token = Separator { content = " " }, state = TopRec {} },        
        Node {  token = Recursive { lit = "recursive let", skiped = [Separator { content = " " }] }, state = Rec {}, sons = [
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
