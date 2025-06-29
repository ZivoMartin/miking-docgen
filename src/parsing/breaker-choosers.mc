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
lang BreakerChooserInterface

    type Breaker = { breakers: [String], state: State }

    sem chooseCrash =
        | (state, word) -> error
                (concatAll ["Parsing failed: You cannot have the word ", word, " inside a ", (toString state), " block."])

    sem topVersionCrash =
        | state -> error (concat "Parsing failed: There is no top version of "(toString state)) 

        
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

    -- Determine the new state and the breakers after having find a block opener
    sem choose : (State, String) -> Breaker
    sem choose =

    -- Determine if for a given breaker, the tokenisation should continue for the parent state
    sem continue : (State, String) -> Bool
    sem continue =
        | (state, word) -> error (concatAll ["Parsing Failed: ", word, " should not be a breaker of ", (toString state), "."])

    -- Determine for a given context if the block become hard or no
    sem isHard: (State, String) -> Bool
    sem isHard =

    -- Determine if the breaker should be part of the current block, or should remain in the stream.
    sem absorbIt : (State, String) -> Bool
    sem absorbIt  =
        | (state, word) ->
            let e = concatAll ["Parsing Failed: ", word, " should not call absorbIt in ", (toString state), " state."] in
            error e

    -- TopVersion if possible the given state to it's top version, crash otherwise.
    sem topVersion : State -> State
    sem topVersion =

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
        | (Program {}, "let") -> build letBreak (TopLet {})
        | (Program {}, "recursive") -> build letBreak (TopLet {})
        | (Program {}, "utest") -> build letBreak (TopUtest {})

        | (Program {}, "lang") -> build ["end"] (Lang {})
        | (Program {}, "mexpr") -> build ["lang", "mexpr"] (Mexpr {})
        | (Program {}, "type") -> build topBreak (TopType {})
        | (Program {}, "con") -> build topBreak (TopCon {})
        | (Program {}, word) -> chooseCrash (Program {}, word)

    sem continue =
        | (Program {}, "") -> false
        | (Program {}, _) -> true

    sem absorbIt =
        | (Program {}, word) -> true

    sem topVersion =
        | Program {} -> topVersionCrash (Program {})
end

lang MexprTopLetUtestBreakerChooser = BreakerChooserInterface
    
    sem choose =
        | (Mexpr {} | TopLet {} | TopUtest {}, "let") -> build fullLetBreak (Let {})
        | (Mexpr {} | TopLet {} | TopUtest {}, "recursive") -> build fullLetBreak (Let {})
        | (Mexpr {} | TopLet {} | TopUtest {}, "utest") -> build fullLetBreak (Utest {})
        | (Mexpr {} | TopLet {} | TopUtest {}, "type") -> build fullTopBreak (Type {})
        | (Mexpr {} | TopLet {} | TopUtest {}, "con") -> build fullTopBreak (Con {})
        | (Mexpr {} | TopLet {} | TopUtest {}, "use") -> build ["in"] (Use {})
        | (Mexpr {} | TopLet {} | TopUtest {}, word) -> chooseCrash (Mexpr {}, word)

    sem continue =
        | (Mexpr {} | TopLet {} | TopUtest {}, _) -> true

    sem isHard =
        | (Mexpr {} | TopLet {} | TopUtest {}, _) -> false

    sem absorbIt =
        | (Mexpr {} | TopLet {} | TopUtest {}, word) -> false

    sem topVersion =
        | (Mexpr {} | TopLet {} | TopUtest {}) & s -> topVersionCrash s
end

        
lang LetUtestBreakerChooser = BreakerChooserInterface

    sem choose =
        | (Let {} | Utest {}, "let") -> build fullLetBreak (Let {})
        | (Let {} | Utest {}, "recursive") -> build fullLetBreak (Let {})
        | (Let {} | Utest {}, "utest") -> build fullTopBreak (Utest {})
        | (Let {} | Utest {}, "type") -> build fullTopBreak (Type {})
        | (Let {} | Utest {}, "con") -> build fullTopBreak (Con {})
        | (Let {} | Utest {}, "use") -> build ["in"] (Use {})    
        | (Let {} | Utest {}, word) -> chooseCrash (Let {}, word)

    sem continue =
        | (Let {} | Utest {}, ("in")) -> true
        | (Let {} | Utest {}, _) -> false

    sem isHard =
        | (Let {} | Utest {}, _) -> true
    
    sem absorbIt =
        | (Let {} | Utest {}, "in") -> true
        | (Let {} | Utest {}, word) -> false

    sem topVersion =
        | Utest {} -> TopUtest {}
        | Let {} -> TopLet {}

end
    
lang TopTypeConBreakerChooser = BreakerChooserInterface
    
    sem choose =
        | (TopType {} | TopCon {}, "use") -> build ["in"] (Use {})
        | (TopType {} | TopCon {}, word) -> chooseCrash (TopType {}, word)

    sem continue =
        | (TopType {} | TopCon {}, _) -> true

    sem isHard =
        | (TopType {} | TopCon {}, _) -> false

    sem absorbIt =
        | (TopType {} | TopCon {}, word) -> false

    sem topVersion =
        | (TopType {} | TopCon {}) & s -> topVersionCrash s
end

lang TypeConBreakerChooser = BreakerChooserInterface

    sem choose =
        | (Type {} | Con {}, "use") -> build ["in"] (Use {})
        | (Type {} | Con {}, word) -> chooseCrash (Type {}, word)

    sem continue =
        | (Type {} | Con {}, "in") -> true
        | (Type {} | Con {}, word) -> false

    sem isHard =
        | (Type {} | Con {}, _) -> true

    sem absorbIt =
        | (Type {} | Con {}, "in") -> true
        | (Type {} | Con {}, word) -> false

    sem topVersion =
        | Type {} -> TopType {}
        | Con {} -> TopCon {}

end

let langBreak = ["end", "sem", "syn"]
let langFullBreak = concat ["type", "con"] langBreak
let langFullBreakIn = concat ["type", "con", "in"] langBreak    

            
lang LangBreakerChooser = BreakerChooserInterface

    sem choose =
        | (Lang {}, "type") -> build langFullBreak (TopType {})
        | (Lang {}, "con") -> build langFullBreak (TopCon {})
        | (Lang {}, "syn") -> build langFullBreak (Syn {})
        | (Lang {}, "sem") -> build langBreak (Sem {})
        | (Lang {}, word) -> chooseCrash (Lang {}, word)
    
    sem absorbIt =
        | (Lang {}, word) -> true

    sem continue =
        | (Lang {}, word) -> true

    sem topVersion =
        | Lang {} -> topVersionCrash (Lang {})

end

lang SemBreakerChooser = BreakerChooserInterface

    sem choose =
        | (Sem {}, "let") -> build ["in"] (Let {})
        | (Sem {}, "recursive") -> build ["in"] (Let {})
        | (Sem {}, "utest") -> build ["in"] (Utest {})

        | (Sem {}, "type") -> build langFullBreakIn (Type {})
        | (Sem {}, "con") -> build langFullBreakIn (Con {})
        | (Sem {}, "syn") -> build langFullBreak (Syn {})
        | (Sem {}, "sem") -> build langBreak (Sem {})
        | (Sem {}, "use") -> build ["in"] (Use {})        
        | (Sem {}, word) -> chooseCrash (Sem {}, word)

    sem continue =
        | (Sem {}, "end") -> false
        | (Sem {}, _) -> true

    sem isHard =
        | (Sem {}, _) -> false
    
    sem absorbIt =
        | (Sem {}, _) -> false

    sem topVersion =
        | Sem {} -> Sem {}

end

lang SynBreakerChooser = BreakerChooserInterface

    sem choose =
        | (Syn {}, "syn") -> build langFullBreak (Syn {})
        | (Syn {}, "sem") -> build langBreak (Sem {})
        | (Syn {}, "use") -> build ["in"] (Use {})    
        | (Syn {}, word) -> chooseCrash (Syn {}, word)

    sem continue =
        | (Syn {}, "end") -> false
        | (Syn {}, _) -> true

    sem isHard =
        | (Syn {}, _) -> false
    
    sem absorbIt =
        | (Syn {}, _) -> false

    sem topVersion =
        | Syn {} -> Syn {}

end

lang UseBreakerChooser = BreakerChooserInterface

    sem choose =
        | (Use {}, "use") -> build ["in"] (Use {})        
        | (Use {}, word) -> chooseCrash (Use {}, word)

    sem continue =
        | (Use {}, word) -> true

    sem isHard =
        | (Use {}, _) -> false
    
    sem absorbIt =
        | (Use {}, "in") -> true
        | (Use {}, word) -> false

    sem topVersion =
        | Use {} -> Use {}
end


    
lang BreakerChooser = ProgramBreakerChooser + MexprTopLetUtestBreakerChooser + LetUtestBreakerChooser + LangBreakerChooser + TopTypeConBreakerChooser + TypeConBreakerChooser + SynBreakerChooser + SemBreakerChooser + UseBreakerChooser end
