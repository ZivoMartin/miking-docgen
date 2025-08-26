-- # BreakerChooser system: choosing breakers and managing parse state
--
-- This module defines a set of `BreakerChoosers`, one per parsing state.
--
-- This file defines the automaton of our parser. The idea is that, depending on the current state,
-- when reading an opener, we don’t want to choose the same breakers or transition into the same states.
--
-- For example, if we are in the `Lang {}` state, we want `end` to be considered as a `type` breaker.
-- Whereas if we are in the `Program` state, it’s unnecessary to consider `end` as a type breaker—even
-- though doing so wouldn’t affect the parser’s validity.
--
-- The core idea is that parsing is guided by a state machine:
-- - The current **state** determines what the parent node is.
-- - A **breaker** is a token that closes one or more blocks.
-- - When a token is seen, the BreakerChooser can decide:
--     * What are the breakers of this new block? (`choose`)
--     * Should parsing continue under the same parent node? (`continue`)
--       For instance, if we have `lang sem A = sem B = end`, the second `sem` will break the first `sem`,
--       but we want to continue parsing under the `lang` parent. On the other hand, when we see `end`,
--       it will break the second `sem`, and we don’t want to continue parsing under the `lang` parent.
--     * Should this breaker be absorbed? (`absorbIt`)
--       In the previous example, the second `sem` will not absorb its breaker (it belongs to the parent block),
--       whereas the `Lang {}` block will absorb the `end`. The automaton decides whether a block in a given state
--       is allowed to absorb its breaker.
--     * Is this a hard break? (`reStructureTree`)
--       We decide whether a given break is hard using the automaton. For example, if a `let` is broken by `lang`,
--       we must restructure the tree and reinterpret that `let` as a `TopLet`.
--     * What is the alternate version of this state? (`switchVersion`)
--       When switching states (e.g., from `Let` to `TopLet`), this decides what the new state should be.
--
-- The whole system is composed at the bottom into a single `BreakerChooser`.


include "./lexing/token-readers.mc"


-- Interface for a BreakerChooser; all choosers implement this contract.
lang BreakerChooserInterface = TokenReader

    -- A breaker is a stack of String representing all the possible breaker
    -- for a block along with the state of this block
    type Breaker = { breakers: [String], state: State }

    -- Parser automaton states (parent context and block type).
    syn State = 
        | StateProgram {}
        | StateTopLet {}
        | StateRecLet {}        
        | StateLet {}
        | StateTopRec {}
        | StateRec {}
        | StateLang {}
        | StateTopType {}
        | StateType {}
        | StateTopUse {}
        | StateUse {}
        | StateSem {}
        | StateSyn {}
        | StateCon {}
        | StateTopCon {}
        | StateMexpr {}
        | StateUtest {}
        | StateTopUtest {}

    -- Human-readable name for a State.
    sem toString: State -> String
    sem toString =
        | StateProgram {} -> "Program"
        | StateTopLet {} -> "TopLet"
        | StateRecLet {} -> "RecLet"        
        | StateLet {} -> "Let"
        | StateLang {} -> "Lang"
        | StateTopRec {} -> "TopRec"
        | StateRec {} -> "Rec"
        | StateTopType {} -> "TopType"
        | StateType {} -> "Type"
        | StateSem {} -> "Sem"
        | StateSyn {} -> "Syn"
        | StateCon {} -> "Con"
        | StateTopCon {} -> "TopCon"
        | StateMexpr {} -> "Mexpr"
        | StateUse {} -> "Use"
        | StateUtest {} -> "Utest"
        | StateTopUtest {} -> "TopUtest"

    -- Builds a uniform crash message with position.
    sem crashMessage : Pos -> String
    sem crashMessage =
        | pos -> join ["Parsing Failed, x: ", int2string pos.x, ", y: ", int2string pos.y, ": "]
    
    -- Determine the new state and the breakers after having find a block opener
    sem choose : (State, String, Pos) -> Breaker
    sem choose =
      | (state, word, pos) -> error (join [crashMessage pos, "You cannot have the word ", word, " inside a ", (toString state), " block."])

    -- Determine if for a given breaker, the tokenisation should continue for the parent state
    sem continue : (State, String) -> Bool
    sem continue =
        | (state, word) -> true

    -- Determine for a given context if the block become hard or no
    sem reStructureTree: (State, String) -> Bool
    sem reStructureTree =
        | (_, _) -> false

    -- Determine if the breaker should be part of the current block, or should remain in the stream.
    sem absorbIt : (State, String) -> Bool
    sem absorbIt  =
        | (state, word) -> false

    -- SwitchVersion if possible the given state to it's top version, crash otherwise.
    sem switchVersion : (State, String) -> State
    sem switchVersion =
        | (state, _) -> state
        
    -- Helper to build a Breaker from a breaker list and a state.
    sem build : [String] -> State -> Breaker
    sem build =
    | breakers -> lam state. { breakers = breakers, state = state }

     
end

-- These are the breakers for any top level element
let topBreak = ["let", "recursive", "con", "lang", "sem", "syn", "type", "mexpr", "utest"]
-- These are the breakers for any top level element, with the in added 
let fullTopBreak = cons "in" topBreak

-- Breaker for a top let
let letBreak = ["lang", "mexpr", "sem", "syn", "#end", "#in"]
-- Breakers for a nested let
let fullLetBreak = cons "in" letBreak
-- Breakers for a rec let, those are the recursive ender token.
let recBreak = ["#in", "#end"]

let topLvlBreak = ["lang", "mexpr"]

let topLvlAnyBreak = concat topLvlBreak ["let", "recursive", "con", "type", "mexpr", "utest"]

lang ProgramBreakerChooser = BreakerChooserInterface
    
    sem choose =
        | (StateProgram {}, "let", pos) -> build topLvlBreak (StateTopLet {})
        | (StateProgram {}, "utest", pos) -> build topLvlBreak (StateTopUtest {})
        | (StateProgram {}, "lang", pos) -> build ["end"] (StateLang {})
        | (StateProgram {}, "mexpr", pos) -> build [] (StateMexpr {}) -- Nothing except eof can break a mexpr
        | (StateProgram {}, "type", pos) -> build topLvlAnyBreak (StateTopType {})
        | (StateProgram {}, "con", pos) -> build topLvlAnyBreak (StateTopCon {})
        | (StateProgram {}, "recursive", pos) -> build ["#end"] (StateTopRec {})

    sem continue =
        | (StateProgram {}, "") -> false

    sem absorbIt =
        | (StateProgram {}, word) -> true

end



lang TopBreakerChooser = BreakerChooserInterface
    
    sem choose =
        | (StateMexpr {} | StateTopLet {} | StateRecLet {} | StateTopUtest {}, "let", pos) -> build fullLetBreak (StateLet {})
        | (StateMexpr {} | StateTopLet {} | StateRecLet {} | StateTopUtest {}, "utest", pos) -> build fullLetBreak (StateUtest {})
        | (StateMexpr {} | StateTopLet {} | StateRecLet {} | StateTopUtest {}, "recursive", pos) -> build recBreak (StateRec {})
        | (StateMexpr {} | StateTopLet {} | StateRecLet {} | StateTopUtest {}, "type", pos) -> build fullTopBreak (StateType {})
        | (StateMexpr {} | StateTopLet {} | StateRecLet {} | StateTopUtest {}, "con", pos) -> build fullTopBreak (StateCon {})
        | (StateMexpr {} | StateTopLet {} | StateRecLet {} | StateTopUtest {}, "use", pos) -> build ["in"] (StateUse {})

    sem continue =
        | (StateRecLet {}, _) -> false

end

    
lang TopRecBreakerChooser = BreakerChooserInterface

    sem choose =
        | (StateTopRec {}, "let", pos) -> build letBreak (StateRecLet {})

    sem absorbIt =
        | (StateTopRec {}, _) -> true
end

lang RecBreakerChooser = BreakerChooserInterface

    sem choose =
        | (StateRec {}, "let", pos) -> build letBreak (StateRecLet {})

    sem absorbIt =
        | (StateRec {}, _) -> true

    sem reStructureTree =
        | (StateRec {}, "#end") -> true

    sem continue =
        | (StateRec {}, "#end") -> false

    sem switchVersion =
        | (StateRec {}, "#end") -> StateTopRec {}
end

            
lang LetUtestBreakerChooser = BreakerChooserInterface

    sem choose =
        | (StateLet {} | StateUtest {}, "let", pos) -> build fullLetBreak (StateLet {})
        | (StateLet {} | StateUtest {}, "utest", pos) -> build fullLetBreak (StateUtest {})
        | (StateLet {} | StateUtest {}, "recursive", pos) -> build recBreak (StateRec {})
        | (StateLet {} | StateUtest {}, "type", pos) -> build fullTopBreak (StateType {})
        | (StateLet {} | StateUtest {}, "con", pos) -> build fullTopBreak (StateCon {})
        | (StateLet {} | StateUtest {}, "use", pos) -> build ["in"] (StateUse {})

    sem continue =
        | (StateLet {} | StateUtest {}, !"in") -> false

    sem reStructureTree =
        | (StateLet {} | StateUtest {}, !"in") -> true
    
    sem absorbIt =
        | (StateLet {} | StateUtest {}, "in") -> true

    sem switchVersion =
        | (StateUtest {}, !"in") -> StateTopUtest {}
        | (StateLet {}, "#in" | "#end") -> StateRecLet {}        
        | (StateLet {}, !"in") -> StateTopLet {}

end
    
lang TopTypeConBreakerChooser = BreakerChooserInterface
    
    sem choose =
        | (StateTopType {} | StateTopCon {}, "use", pos) -> build ["in"] (StateUse {})

end

lang TypeConBreakerChooser = BreakerChooserInterface

    sem choose =
        | (StateType {} | StateCon {}, "use", pos) -> build ["in"] (StateUse {})

    sem continue =
        | (StateType {} | StateCon {}, !"in") -> false

    sem reStructureTree =
        | (StateType {} | StateCon {}, !"in") -> true

    sem absorbIt =
        | (StateType {} | StateCon {}, "in") -> true

    sem switchVersion =
        | (StateType {}, !"in") -> StateTopType {}
        | (StateCon {}, !"in") -> StateTopCon {}

end

let langBreak = ["end", "sem", "syn"]
let langFullBreak = concat ["type", "con"] langBreak
let langFullBreakIn = concat ["type", "con", "in"] langBreak    

            
lang LangBreakerChooser = BreakerChooserInterface

    sem choose =
        | (StateLang {}, "type", pos) -> build langFullBreak (StateTopType {})
        | (StateLang {}, "con", pos) -> build langFullBreak (StateTopCon {})
        | (StateLang {}, "syn", pos) -> build langFullBreak (StateSyn {})
        | (StateLang {}, "sem", pos) -> build langBreak (StateSem {})
    
    sem absorbIt =
        | (StateLang {}, word) -> true

end

lang SemBreakerChooser = BreakerChooserInterface

    sem choose =
        | (StateSem {}, "let", pos) -> build ["in"] (StateLet {})
        | (StateSem {}, "recursive", pos) -> build recBreak (StateRec {})
        | (StateSem {}, "utest", pos) -> build ["in"] (StateUtest {})
        | (StateSem {}, "type", pos) -> build langFullBreakIn (StateType {})
        | (StateSem {}, "con", pos) -> build langFullBreakIn (StateCon {})
        | (StateSem {}, "syn", pos) -> build langFullBreak (StateSyn {})
        | (StateSem {}, "sem", pos) -> build langBreak (StateSem {})
        | (StateSem {}, "use", pos) -> build ["in"] (StateUse {})        

    sem continue =
        | (StateSem {}, "end") -> false

end

lang SynBreakerChooser = BreakerChooserInterface

    sem choose =
        | (StateSyn {}, "syn", pos) -> build langFullBreak (StateSyn {})
        | (StateSyn {}, "sem", pos) -> build langBreak (StateSem {})
        | (StateSyn {}, "use", pos) -> build ["in"] (StateUse {})

    sem continue =
        | (StateSyn {}, "end") -> false
    
end

lang UseBreakerChooser = BreakerChooserInterface

    sem choose =
        | (StateUse {}, "use", pos) -> build ["in"] (StateUse {})

    sem absorbIt =
        | (StateUse {}, "in") -> true
end


    
lang BreakerChooser = ProgramBreakerChooser + RecBreakerChooser + TopRecBreakerChooser + TopBreakerChooser + LetUtestBreakerChooser + LangBreakerChooser + TopTypeConBreakerChooser + TypeConBreakerChooser + SynBreakerChooser + SemBreakerChooser + UseBreakerChooser end
