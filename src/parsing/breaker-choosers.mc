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
-- Each function has a default implementation allowing us to not define
-- all sems if not necessary.
lang BreakerChooserInterface = TokenReader

    -- A breaker is a set of String representing all the possible breaker
    -- for a block along with the state of this block
    type Breaker = { breakers: [String], state: State }

    -- Parser automaton states.
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


    -- Determine the new state and the breakers after having find a block opener.
    -- The default behavior is to crash since it would mean the automaton is wrong.
    -- Indeed, for a valid automaton all cases should be handled.
    sem choose : (State, String, Pos) -> Breaker
    sem choose =
      | (state, word, pos) -> error (join ["Parsing Failed, x: ", int2string pos.x, ", y: ", int2string pos.y, ": ", "You cannot have the word ", word, " inside a ", (toString state), " block."])

    -- Determine if for a given breaker, the tokenisation should continue for the parent state.
    -- The default behavior is to continue the tokenisation.
    sem continue : (State, String) -> Bool
    sem continue =
        | (state, word) -> true

    -- Determine for a given context if the block become hard or no.
    -- The default behavior is to not restructure the tree.
    sem reStructureTree: (State, String) -> Bool
    sem reStructureTree =
        | (_, _) -> false

    -- Determine if the breaker should be part of the current block, or should be part of the bloc.
    -- The default behavior is to reject the breaker.
    sem absorbIt : (State, String) -> Bool
    sem absorbIt  =
        | (state, word) -> false

    -- SwitchVersion will take a state and its breaker and will
    -- return the actual version of the state if it has to change.
    -- The default behavior is to keep the same state.
    sem switchVersion : (State, String) -> State
    sem switchVersion =
        | (state, _) -> state
        
    -- Helper to build a Breaker from a breaker list and a state.
    sem build : [String] -> State -> Breaker
    sem build =
    | breakers -> lam state. { breakers = breakers, state = state }

     
end

-- Defining some constants for the breakers states

-- This bloc can only close on in.
let innerBloc = ["in"]

-- Breakers for a rec objects, those are the lit representation of recursive enders.
let recBreak = ["#in", "#end"]

-- Breaker for a recursive that can only end will "in"
let recInner = ["#in"]

-- Breaker for a top level recursive.
let recOuter = ["#end"]

-- This breakers are for the top level objects,
-- indeed we cannot consider let, type, .. as explicit breaker
-- since they could be nested.
let topLvlBreak = ["lang", "mexpr"]

-- This breakers are for top level that do not have any nested blocs,
-- so we can instantly break on any opener as we know they are not nested.
let topLvlAnyBreak = concat topLvlBreak ["let", "recursive", "con", "type", "mexpr", "utest"]

-- Same as topLvlBreak, but when the bloc is possibly a nested bloc and so can end with "in"
let innerCandidateBreak = cons "in" topLvlBreak

-- Same as topLvlAnyBreak, but when the bloc is possibly a nested bloc and so can end with "in"
let innerCandidateAnyBreak = cons "in" topLvlAnyBreak

-- All top level lang keywords that can only break, syntaxically
-- its impossible that this blocs are nested.
-- For exemple we cannot have a sem inside a sem.
let langBreak = ["end", "sem", "syn"]

-- Here we also consider type and con that are also top level keyword
-- for langs, but are a bit special as they could be nested.
let langFullBreak = concat ["type", "con"] langBreak

-- And finally, here we also consider the "in"
-- We will use this one if the bloc could be nested.
let langFullBreakIn = cons "in" langFullBreak 

-- We are here at the root of the program, this implies we can find any top level bloc,
-- And moreover, we can assume with 100% chances that each bloc is a top level.
lang ProgramBreakerChooser = BreakerChooserInterface

     -- The choose implementation has 4 main cases:
     -- 1. type and con are very simple bloc without any nested blocs, so anything will break them without any ambiguity
     -- 2. lang and recursive are also quite simple since they are closed with only one word. Note that #end is the litteral form of RecursiveEnder.
     -- 3. let and utest are a bit more restricted, indeed we cant consider let as a breaker since it could also be a nested break, so
     --  the only breakers are words that can only exist on the top level.
     -- 4. mexpr is the easiest case as it cant break, as it is the last top level bloc.
    sem choose =        
        | (StateProgram {}, "type", pos) -> build topLvlAnyBreak (StateTopType {})
        | (StateProgram {}, "con", pos) -> build topLvlAnyBreak (StateTopCon {})

        | (StateProgram {}, "lang", pos) -> build ["end"] (StateLang {})
        | (StateProgram {}, "recursive", pos) -> build recOuter (StateTopRec {})

        | (StateProgram {}, "let", pos) -> build topLvlBreak (StateTopLet {})
        | (StateProgram {}, "utest", pos) -> build topLvlBreak (StateTopUtest {})

        | (StateProgram {}, "mexpr", pos) -> build [] (StateMexpr {})

    -- We can't not absorb any word here as it is the top level.
    sem absorbIt =
        | (StateProgram {}, word) -> true

end

-- We handle here the mexpr case, this is quite simple as we know
-- that all its component will never be top level and nothing can break it.
lang MexprBreakerChooser = BreakerChooserInterface

    -- As every blocs are inner blocs, we can just assume a in will come close them.
    -- Only recursive is closed with #in, the RecursiveEnder lit for in.
    sem choose =
    | (StateMexpr {}, "let", pos) -> build innerBloc (StateLet {})
    | (StateMexpr {}, "utest", pos) -> build innerBloc (StateUtest {})
    | (StateMexpr {}, "type", pos) -> build ["in"] (StateType {})
    | (StateMexpr {}, "con", pos) -> build ["in"] (StateCon {})
    | (StateMexpr {}, "use", pos) -> build ["in"] (StateUse {})

    | (StateMexpr {}, "recursive", pos) -> build recInner (StateRec {})
    
end

-- In this lang we handle both top and inner let and utests.
-- The main idea here is that when a TopLet find a let it can't know beforehand
-- if it is an inner let or not. So it assume it is and give it the possibility to break
-- on "in", if later the let breaks on a top level keyword such as lang or mexpr, it
-- will triggers a reconstruction.
-- So basically, all Let can possibly be TopLet, and so we need to apply the same rules when
-- we meet "let" as a Let. Utests work exactly the same.
lang LetUtestBreakerChooser = BreakerChooserInterface

    -- As for the Program bloc we have 4 cases:
    -- 1. let and utest are still very ambiguous but can this time break on a in.
    -- 2. For type and con it is the same logic, but we can this time also break on in.
    -- 3. Recursive can this time break on both "#end" and "#in" as we cant know beforehand if it is a toplvl or no.
    -- 4. And use will alway and with in.
    sem choose =
        | (StateLet {} | StateUtest {} | StateTopLet {} | StateTopUtest {}, "let", pos) -> build innerCandidateBreak (StateLet {})
        | (StateLet {} | StateUtest {} | StateTopLet {} | StateTopUtest {}, "utest", pos) -> build innerCandidateBreak (StateUtest {})

        | (StateLet {} | StateUtest {} | StateTopLet {} | StateTopUtest {}, "type", pos) -> build innerCandidateAnyBreak (StateType {})
        | (StateLet {} | StateUtest {} | StateTopLet {} | StateTopUtest {}, "con", pos) -> build innerCandidateAnyBreak (StateCon {})
        
        | (StateLet {} | StateUtest {} | StateTopLet {} | StateTopUtest {}, "recursive", pos) -> build recBreak (StateRec {})
        
        | (StateLet {} | StateUtest {} | StateTopLet {} | StateTopUtest {}, "use", pos) -> build innerBloc (StateUse {})

    -- For an inner bloc, we only want to continue the parent
    -- if the bloc is indeed an inner bloc, meaning the breaker is in.
    -- For TopLvl blocs, we always have to continue as their parent is always Program.
    sem continue =
        | (StateLet {} | StateUtest {}, !"in") -> false

    -- If we close an inner with anything else than an in, it means it
    -- is not an inner, and so we want to trigger a restructuration.
    sem reStructureTree =
        | (StateLet {} | StateUtest {}, !"in") -> true

    -- The let will only aborb in, all others breakers are generally
    -- first words of new bloc such as lang.
    sem absorbIt =
        | (StateLet {} | StateUtest {}, "in") -> true

    -- As alway if we do not end with in we want to cast inner
    -- let and utest in top lvl, the little changment here is if
    -- we break on a recursive ender as an in, it would mean that the
    -- the let was actually a RecLet.
    sem switchVersion =
        | (StateUtest {}, !"in") -> StateTopUtest {}
        | (StateLet {}, "#in" | "#end") -> StateRecLet {}        
        | (StateLet {}, !"in") -> StateTopLet {}



end

-- Here we handle the LetRec case that is quite similare as TopBreakerChooser
-- But it is easier since only let blocs are ambiguous.
lang LetRecBreakerChooser = BreakerChooserInterface

    -- So here we have 3 cases
    -- 1. let is still hard as we cant know if it is a RecLet or nested Let, so we put both in and RecursiveEnder and assume its a nested, will fix later if its not.
    -- 2. utest, type, con, use are all very simple, as we are inside a recursive they can't be anything else than inner blocs.
    -- 3. And finally, recursive can also only be an inner block, so we only need one breaker.
    sem choose =
        | (StateRecLet {}, "let", pos) -> build ["in", "#end", "#in"] (StateLet {})
        
        | (StateRecLet {}, "utest", pos) -> build innerBloc (StateUtest {})
        | (StateRecLet {}, "type", pos) -> build innerBloc (StateType {})
        | (StateRecLet {}, "con", pos) -> build innerBloc (StateCon {})
        | (StateRecLet {}, "use", pos) -> build innerBloc (StateUse {})
        
        | (StateRecLet {}, "recursive", pos) -> build recInner (StateRec {})

    -- We never want to continue a RecLet that broke as its only breaker is #end and #in that
    -- is also breaking the recursive parent bloc.
    sem continue =
        | (StateRecLet {}, _) -> false
end

-- Here is the langage for Rec bloc, including the TopRec case and ambiguous inner case.
lang RecBreakerChooser = BreakerChooserInterface

    -- recursive blocs can only open on let, so when we see one we know its a RecLet.
    -- The TopRec do not need to include "#in" in the breaker since we know it will
    -- end with "#end"
    sem choose =
        | (StateTopRec {}, "let", pos) -> build recOuter (StateRecLet {})
        | (StateRec {}, "let", pos) -> build recBreak (StateRecLet {})
        
    -- Rec blocs will end with #end or #in, and we want to absorb it as it is part of the bloc.
    sem absorbIt =
        | (StateRec {} | StateTopRec {}, _) -> true

    -- If the Rec bloc ends with and #end, it means it was a TopRec, so we want to trigger
    -- a reconstruction.
    sem reStructureTree =
        | (StateRec {}, "#end") -> true

    -- At this point, if we find an #end, it means our curent parent is not our real parent.
    -- So we want to stop its execution and re-structure the tree correctly.
    sem continue =
        | (StateRec {}, "#end") -> false

    -- Here again, if the Rec ends with end, we want to cast it into TopRec.
    sem switchVersion =
        | (StateRec {}, "#end") -> StateTopRec {}
end
    
-- This language handles both con and type that are exactly the same.
-- A top lvl con or type will be easy to handle as it can only contain use
-- blocs, that only break on in.
-- The inner version is not really harder, since it still not contain any sub-bloc,
-- but we still have to handle potential restructuration.
lang TypeConBreakerChooser = BreakerChooserInterface

    sem choose =
        | (StateType {} | StateCon {} | StateTopType {} | StateTopCon {}, "use", pos) -> build innerBloc (StateUse {})

    -- If it does not break on in, it means it was actually a top lvl type and
    -- has to break the parent node.
    sem continue =
        | (StateType {} | StateCon {}, !"in") -> false

    -- If we find a in, we have to trigger the restructuration as
    -- the inner assumption was false.
    sem reStructureTree =
        | (StateType {} | StateCon {}, !"in") -> true

    -- We only want to absorb "in", other breakers are not part of the bloc.
    sem absorbIt =
        | (StateType {} | StateCon {}, "in") -> true

    -- And again, if we break on something else than in, we cast in top level.
    sem switchVersion =
        | (StateType {}, !"in") -> StateTopType {}
        | (StateCon {}, !"in") -> StateTopCon {}

end   

-- In this langauge we handle the "lang" blocs.
-- As a reminder, top level keyword in lang blocs can only
-- be sem, syn, con, type and end that is closing the langage.
lang LangBreakerChooser = BreakerChooserInterface

    -- type con and syn does not have any nested bloc, so
    -- we can simply break on any top level lang bloc.
    -- Sem can have some nested bloc, and most importantly can contain
    -- "type" and "con" definition, so we cannot break on type and con
    -- and will need to do assumtions.
    sem choose =
        | (StateLang {}, "type", pos) -> build langFullBreak (StateTopType {})
        | (StateLang {}, "con", pos) -> build langFullBreak (StateTopCon {})
        | (StateLang {}, "syn", pos) -> build langFullBreak (StateSyn {})
        
        | (StateLang {}, "sem", pos) -> build langBreak (StateSem {})

    -- lang can only break on end, and we want to absorb this breaker.
    sem absorbIt =
        | (StateLang {}, _) -> true

end


-- This langage handles both syn and sem.
lang SemSynBreakerChooser = BreakerChooserInterface


    -- This implementation of choose
    -- can be a bit confusing as syn do not have any choose cases except use, but it is
    -- due to the fact that all opener that syn might find
    -- are breaker of syn, so it will never have to choose
    -- anything.
    -- On the other hand, sem have to handle 3 cases:
    -- 1. let and utest are this time quite easy as they are 100% inner blocs, so we
    --    cas just consider them as inner. Use as always is closed with only "in".
    -- 2. type and con are a bit more difficult snice we cant know if they are
    --    top level or no, so we just assume they are no and give them the possibility to break on "in"
    -- 3. And finally, recursive is 100% an inner bloc and so we cann break it on #in.
    sem choose =
        | (StateSem {}, "let", pos) -> build innerBloc (StateLet {})
        | (StateSem {}, "utest", pos) -> build innerBloc (StateUtest {})
        | (StateSem {} | StateSyn {}, "use", pos) -> build innerBloc (StateUse {})
        
        
        | (StateSem {}, "type", pos) -> build langFullBreakIn (StateType {})
        | (StateSem {}, "con", pos) -> build langFullBreakIn (StateCon {})

        | (StateSem {}, "recursive", pos) -> build recInner (StateRec {})

    -- We always want to continue the parent bloc that is the lang except
    -- if we break on "end", the breaker of the lang.
    sem continue =
        | (StateSem {} | StateSyn {}, "end") -> false

end


-- Use is from far the easiest langage, as it do not contain
-- nested bloc and always break on in.
-- We do not even need any chose implementation.
lang UseBreakerChooser = BreakerChooserInterface

    -- We just want to absorb the "in" breaker
    sem absorbIt =
        | (StateUse {}, "in") -> true
end


-- Final BreakerChooser unifying all the Breakers.    
lang BreakerChooser = ProgramBreakerChooser + RecBreakerChooser + LetUtestBreakerChooser + LangBreakerChooser + TypeConBreakerChooser + SemSynBreakerChooser + UseBreakerChooser + MexprBreakerChooser + LetRecBreakerChooser end
