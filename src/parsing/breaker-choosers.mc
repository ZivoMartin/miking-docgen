-- # BreakerChooser system: choosing breakers and managing parse state
--
-- This module defines a set of "BreakerChoosers", one per parsing state.
--
-- The core idea is that parsing is guided by a "state machine":  
-- - The current **state** determines which tokens can appear next  
-- - A "breaker" is a token that closes one or more blocks  
-- - When a token is seen, the BreakerChooser decides:
--     * Does this token start a new block? (choose)
--     * Does parsing continue in this block? (continue)
--     * Is this a hard break? (isHard)
--     * Should this breaker be absorbed? (absorbIt)
--     * What is the top version of this state? (topVersion)
--
-- Each concrete BreakerChooser implements this logic for one state:  
-- Program, Let, Lang, Type, Sem, Syn, Con, Use, Mexpr, etc.
--
-- The whole system is composed at the bottom into one `BreakerChooser`.

include "token-readers.mc"


-- Interface for a BreakerChoser, they will all implement it. 
lang BreakerChooserInterface = TokenReader

    type Breaker = { breakers: [String], state: State }
        
    syn State = 
        | Program {}
        | TopLet {}
        | Let {}
        | Lang {}
        | TopType {}
        | Type {}
        | TopUse {}
        | Use {}
        | Sem {}
        | Syn {}
        | Con {}
        | TopCon {}
        | Mexpr {}
        | Utest {}
        | TopUtest {}

    sem toString: State -> String
    sem toString =
        | Program {} -> "Program"
        | TopLet {} -> "TopLet"
        | Let {} -> "Let"
        | Lang {} -> "Lang"
        | TopType {} -> "TopType"
        | Type {} -> "Type"
        | Sem {} -> "Sem"
        | Syn {} -> "Syn"
        | Con {} -> "Con"
        | TopCon {} -> "TopCon"
        | Mexpr {} -> "Mexpr"
        | Use {} -> "Use"
        | Utest {} -> "Utest"
        | TopUtest {} -> "TopUtest"

    sem crashMessage : Pos -> String
    sem crashMessage =
        | pos -> concatAll ["Parsing Failed, x: ", int2string pos.x, ", y: ", int2string pos.y, ": "]
    
    -- Determine the new state and the breakers after having find a block opener
    sem choose : (State, String, Pos) -> Breaker
    sem choose =
      | (state, word, pos) -> error (concatAll [crashMessage pos, "You cannot have the word ", word, " inside a ", (toString state), " block."])

    -- Determine if for a given breaker, the tokenisation should continue for the parent state
    sem continue : (State, String, Pos) -> Bool
    sem continue =
        | (state, word, pos) -> error (concatAll [crashMessage pos, word, " should not be a breaker of ", (toString state), "."])

    -- Determine for a given context if the block become hard or no
    sem isHard: (State, String) -> Bool
    sem isHard =

    -- Determine if the breaker should be part of the current block, or should remain in the stream.
    sem absorbIt : (State, String, Pos) -> Bool
    sem absorbIt  =
        | (state, word, pos) -> error (concatAll [crashMessage pos, word, " should not call absorbIt in ", (toString state), " state."])

    -- TopVersion if possible the given state to it's top version, crash otherwise.
    sem topVersion : (State, Pos) -> State
    sem topVersion =
        | (state, pos) -> error (concatAll [crashMessage pos, "There is no top version of ", toString state])

    sem build : [String] -> State -> Breaker
    sem build =
    | breakers -> lam state. { breakers = breakers, state = state }

     
end
    
let topBreak = ["let", "recursive", "con", "lang", "sem", "syn", "type", "mexpr", "utest"]
let fullTopBreak = cons "in" topBreak

let letBreak = ["lang", "mexpr"]
let fullLetBreak = cons "in" letBreak
    
lang ProgramBreakerChooser = BreakerChooserInterface
    
    sem choose =
        | (Program {}, "let", pos) -> build letBreak (TopLet {})
        | (Program {}, "recursive", pos) -> build letBreak (TopLet {})
        | (Program {}, "utest", pos) -> build letBreak (TopUtest {})

        | (Program {}, "lang", pos) -> build ["end"] (Lang {})
        | (Program {}, "mexpr", pos) -> build ["lang", "mexpr"] (Mexpr {})
        | (Program {}, "type", pos) -> build topBreak (TopType {})
        | (Program {}, "con", pos) -> build topBreak (TopCon {})

    sem continue =
        | (Program {}, "", pos) -> false
        | (Program {}, _, pos) -> true

    sem absorbIt =
        | (Program {}, word, pos) -> true

end

lang MexprTopLetUtestBreakerChooser = BreakerChooserInterface
    
    sem choose =
        | (Mexpr {} | TopLet {} | TopUtest {}, "let", pos) -> build fullLetBreak (Let {})
        | (Mexpr {} | TopLet {} | TopUtest {}, "recursive", pos) -> build fullLetBreak (Let {})
        | (Mexpr {} | TopLet {} | TopUtest {}, "utest", pos) -> build fullLetBreak (Utest {})
        | (Mexpr {} | TopLet {} | TopUtest {}, "type", pos) -> build fullTopBreak (Type {})
        | (Mexpr {} | TopLet {} | TopUtest {}, "con", pos) -> build fullTopBreak (Con {})
        | (Mexpr {} | TopLet {} | TopUtest {}, "use", pos) -> build ["in"] (Use {})
    
    sem continue =
        | (Mexpr {} | TopLet {} | TopUtest {}, _, pos) -> true

    sem isHard =
        | (Mexpr {} | TopLet {} | TopUtest {}, _) -> false

    sem absorbIt =
        | (Mexpr {} | TopLet {} | TopUtest {}, word, pos) -> false

end

        
lang LetUtestBreakerChooser = BreakerChooserInterface

    sem choose =
        | (Let {} | Utest {}, "let", pos) -> build fullLetBreak (Let {})
        | (Let {} | Utest {}, "recursive", pos) -> build fullLetBreak (Let {})
        | (Let {} | Utest {}, "utest", pos) -> build fullTopBreak (Utest {})
        | (Let {} | Utest {}, "type", pos) -> build fullTopBreak (Type {})
        | (Let {} | Utest {}, "con", pos) -> build fullTopBreak (Con {})
        | (Let {} | Utest {}, "use", pos) -> build ["in"] (Use {})    

    sem continue =
        | (Let {} | Utest {}, ("in"), pos) -> true
        | (Let {} | Utest {}, _, pos) -> false

    sem isHard =
        | (Let {} | Utest {}, _) -> true
    
    sem absorbIt =
        | (Let {} | Utest {}, "in", pos) -> true
        | (Let {} | Utest {}, word, pos) -> false

    sem topVersion =
        | (Utest {}, pos) -> TopUtest {}
        | (Let {}, pos) -> TopLet {}

end
    
lang TopTypeConBreakerChooser = BreakerChooserInterface
    
    sem choose =
        | (TopType {} | TopCon {}, "use", pos) -> build ["in"] (Use {})

    sem continue =
        | (TopType {} | TopCon {}, _, pos) -> true

    sem isHard =
        | (TopType {} | TopCon {}, _) -> false

    sem absorbIt =
        | (TopType {} | TopCon {}, word, pos) -> false

end

lang TypeConBreakerChooser = BreakerChooserInterface

    sem choose =
        | (Type {} | Con {}, "use", pos) -> build ["in"] (Use {})

    sem continue =
        | (Type {} | Con {}, "in", pos) -> true
        | (Type {} | Con {}, word, pos) -> false

    sem isHard =
        | (Type {} | Con {}, _) -> true

    sem absorbIt =
        | (Type {} | Con {}, "in", pos) -> true
        | (Type {} | Con {}, word, pos) -> false

    sem topVersion =
        | (Type {}, pos) -> TopType {}
        | (Con {}, pos) -> TopCon {}

end

let langBreak = ["end", "sem", "syn"]
let langFullBreak = concat ["type", "con"] langBreak
let langFullBreakIn = concat ["type", "con", "in"] langBreak    

            
lang LangBreakerChooser = BreakerChooserInterface

    sem choose =
        | (Lang {}, "type", pos) -> build langFullBreak (TopType {})
        | (Lang {}, "con", pos) -> build langFullBreak (TopCon {})
        | (Lang {}, "syn", pos) -> build langFullBreak (Syn {})
        | (Lang {}, "sem", pos) -> build langBreak (Sem {})
    
    sem absorbIt =
        | (Lang {}, word, pos) -> true

    sem continue =
        | (Lang {}, word, pos) -> true


end

lang SemBreakerChooser = BreakerChooserInterface

    sem choose =
        | (Sem {}, "let", pos) -> build ["in"] (Let {})
        | (Sem {}, "recursive", pos) -> build ["in"] (Let {})
        | (Sem {}, "utest", pos) -> build ["in"] (Utest {})

        | (Sem {}, "type", pos) -> build langFullBreakIn (Type {})
        | (Sem {}, "con", pos) -> build langFullBreakIn (Con {})
        | (Sem {}, "syn", pos) -> build langFullBreak (Syn {})
        | (Sem {}, "sem", pos) -> build langBreak (Sem {})
        | (Sem {}, "use", pos) -> build ["in"] (Use {})        

    sem continue =
        | (Sem {}, "end", pos) -> false
        | (Sem {}, _, pos) -> true

    sem isHard =
        | (Sem {}, _) -> false
    
    sem absorbIt =
        | (Sem {}, word, pos) -> false

    sem topVersion =
        | (Sem {}, pos) -> Sem {}

end

lang SynBreakerChooser = BreakerChooserInterface

    sem choose =
        | (Syn {}, "syn", pos) -> build langFullBreak (Syn {})
        | (Syn {}, "sem", pos) -> build langBreak (Sem {})
        | (Syn {}, "use", pos) -> build ["in"] (Use {})    

    sem continue =
        | (Syn {}, "end", pos) -> false
        | (Syn {}, _, pos) -> true

    sem isHard =
        | (Syn {}, _) -> false
    
    sem absorbIt =
        | (Syn {}, word, pos) -> false

    sem topVersion =
        | (Syn {}, pos) -> Syn {}

end

lang UseBreakerChooser = BreakerChooserInterface

    sem choose =
        | (Use {}, "use", pos) -> build ["in"] (Use {})

    sem continue =
        | (Use {}, word, pos) -> true

    sem isHard =
        | (Use {}, _) -> false

    sem absorbIt =
        | (Use {}, "in", pos) -> true
        | (Use {}, word, pos) -> false

    sem topVersion : (State, Pos) -> State
    sem topVersion =
        | (Use {}, pos) -> Use {}
end


    
lang BreakerChooser = ProgramBreakerChooser + MexprTopLetUtestBreakerChooser + LetUtestBreakerChooser + LangBreakerChooser + TopTypeConBreakerChooser + TypeConBreakerChooser + SynBreakerChooser + SemBreakerChooser + UseBreakerChooser end
