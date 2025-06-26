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

    
    sem chooseCrash /- (State, String) -> () -/ =
        | (state, word) -> error
                (concatAll ["Parsing failed: You cannot have the word ", word, " inside a ", (toString state), " block."])
    sem topVersionCrash /- (State) -> () -/ =
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
    sem choose /- (State, String) -> Breaker -/ =
    -- Determine if for a given breaker, the tokenisation should continue for the parent state
    sem continue /- (State, String) -> Bool -/ =
        | (state, word) -> error (concatAll ["Parsing Failed: ", word, " should not be a breaker of ", (toString state), "."])
    -- Determine for a given context if the block become hard or no
    sem isHard /- (State, String) -> Bool -/ =
    -- Determine if the breaker should be part of the current block, or should remain in the stream.
    sem absorbIt /- (State, String) -> Bool -/ =
        | (state, word) -> error (concatAll ["Parsing Failed: ", word, " should not call absorbIt in ", (toString state), " state."])
    -- TopVersion if possible the given state to it's top version, crash otherwise.
    sem topVersion /- State -> State -/ = 
end


let topBreak = ["let", "recursive", "con", "lang", "sem", "syn", "type", "mexpr", "utest"]
let fullTopBreak = cons "in" topBreak

let letBreak = ["lang", "mexpr"]
let fullLetBreak = cons "in" letBreak
    
lang ProgramBreakerChooser = BreakerChooserInterface

    sem choose =
        | (Program {}, "let") -> { breakers = letBreak, state = TopLet {} }
        | (Program {}, "recursive") -> { breakers = letBreak, state = TopLet {} }
        | (Program {}, "utest") -> { breakers = letBreak, state = TopUtest {} }

        | (Program {}, "lang") -> { breakers = ["end"], state = Lang {} }
        | (Program {}, "mexpr") -> { breakers = ["lang", "mexpr"], state = Mexpr {} }
        | (Program {}, "type") -> { breakers = topBreak, state = TopType {} }
        | (Program {}, "con") -> { breakers = topBreak, state = TopCon {} }
        | (Program {}, word) -> chooseCrash (Program {}, word)

    sem continue =
        | (Program {}, "") -> false
        | (Program {}, _) -> true

    sem absorbIt =
        | (Program {}, word) -> true

    sem topVersion =
        | Program {} -> topVersionCrash (Program {})
end

lang MexprBreakerChooser = BreakerChooserInterface
    
    sem choose =
        | (Mexpr {}, "let") -> { breakers = fullLetBreak, state = Let {} }
        | (Mexpr {}, "recursive") -> { breakers = fullLetBreak, state = Let {} }
        | (Mexpr {}, "utest") -> { breakers = fullLetBreak, state = Utest {} }

        | (Mexpr {}, "type") -> { breakers = fullTopBreak, state = Type {} }
        | (Mexpr {}, "con") -> { breakers = fullTopBreak, state = Con {} }
        | (Mexpr {}, "use") -> { breakers = ["in"], state = Use {} }
        | (Mexpr {}, word) -> chooseCrash (Mexpr {}, word)

    sem continue =
        | (Mexpr {}, _) -> true

    sem isHard =
        | (Mexpr {}, _) -> false

    sem absorbIt =
        | (Mexpr {}, word) -> false

    sem topVersion =
        | Mexpr {} -> topVersionCrash (Mexpr {})
end

        
lang TopLetBreakerChooser = BreakerChooserInterface
    
    sem choose =
        | (TopLet {}, "let") -> { breakers = fullLetBreak, state = Let {} }
        | (TopLet {}, "recursive") -> { breakers = fullLetBreak, state = Let {} }
        | (TopLet {}, "utest") -> { breakers = fullLetBreak, state = Utest {} }

        | (TopLet {}, "type") -> { breakers = fullTopBreak, state = Type {} }
        | (TopLet {}, "con") -> { breakers = fullTopBreak, state = Con {} }
        | (TopLet {}, "use") -> { breakers = ["in"], state = Use {} }    
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
        | (Let {}, "let") -> { breakers = fullLetBreak, state = Let {} }
        | (Let {}, "recursive") -> { breakers = fullLetBreak, state = Let {} }
        | (Let {}, "utest") -> { breakers = fullTopBreak, state = Utest {} }

        | (Let {}, "type") -> { breakers = fullTopBreak, state = Type {} }
        | (Let {}, "con") -> { breakers = fullTopBreak, state = Con {} }
        | (Let {}, "use") -> { breakers = ["in"], state = Use {} }    
        | (Let {}, word) -> chooseCrash (Let {}, word)

    sem continue =
        | (Let {}, ("in")) -> true
        | (Let {}, _) -> false

    sem isHard =
        | (Let {}, _) -> true
    
    sem absorbIt =
        | (Let {}, "in") -> true
        | (Let {}, word) -> false

    sem topVersion =
        | Let {} -> TopLet {}

end
    
lang TopTypeBreakerChooser = BreakerChooserInterface
    
    sem choose =
        | (TopType {}, "use") -> { breakers = ["in"], state = Use {} }
        | (TopType {}, word) -> chooseCrash (TopType {}, word)

    sem continue =
        | (TopType {}, _) -> true

    sem isHard =
        | (TopType {}, _) -> false

    sem absorbIt =
        | (TopType {}, word) -> false

    sem topVersion =
        | TopType {} -> topVersionCrash (TopType {})
end
    
lang TypeBreakerChooser = BreakerChooserInterface

    sem choose =
        | (Type {}, "use") -> { breakers = ["in"], state = Use {} }
        | (Type {}, word) -> chooseCrash (Type {}, word)

    sem continue =
        | (Type {}, "in") -> true
        | (Type {}, word) -> false

    sem isHard =
        | (Type {}, _) -> true
        | (Type {}, "in") -> warn "Should be unreachable to ask the state machine if finding a 'in' bound to a 'type' makes the block hard."; true
    
    sem absorbIt =
        | (Type {}, "in") -> true
        | (Type {}, word) -> false

    sem topVersion =
        | Type {} -> TopType {}

end

let langBreak = ["end", "sem", "syn"]
let langFullBreak = concat ["type", "con"] langBreak

            
lang LangBreakerChooser = BreakerChooserInterface

    sem choose =
        | (Lang {}, "type") -> { breakers = langFullBreak, state = TopType {} }
        | (Lang {}, "con") -> { breakers = langFullBreak, state = TopCon {} }
        | (Lang {}, "syn") -> { breakers = langBreak, state = Syn {} }
        | (Lang {}, "sem") -> { breakers = langBreak, state = Sem {} }
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
        | (Sem {}, "let") -> { breakers = ["in"], state = Let {} }
        | (Sem {}, "recursive") -> { breakers = ["in"], state = Let {} }
        | (Sem {}, "utest") -> { breakers = ["in"], state = Utest {} }

        | (Sem {}, "type") -> { breakers = cons "in" langFullBreak, state = Type {} }
        | (Sem {}, "con") -> { breakers = cons "in" langFullBreak, state = Con {} }
        | (Sem {}, "syn") -> { breakers = langFullBreak, state = Syn {} }
        | (Sem {}, "sem") -> { breakers = langBreak, state = Sem {} }
        | (Sem {}, "use") -> { breakers = ["in"], state = Use {} }        
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
        | (Syn {}, "syn") -> { breakers = langBreak, state = Syn {} }
        | (Syn {}, "sem") -> { breakers = langFullBreak, state = Sem {} }
        | (Syn {}, "use") -> { breakers = ["in"], state = Use {} }    
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



lang TopConBreakerChooser = BreakerChooserInterface
    
    sem choose =
        | (TopCon {}, "use") -> { breakers = ["in"], state = Use {} }    
        | (TopCon {}, word) -> chooseCrash (TopCon {}, word)

    sem continue =
        | (TopCon {}, _) -> true

    sem isHard =
        | (TopCon {}, _) -> false

    sem absorbIt =
        | (TopCon {}, word) -> false

    sem topVersion =
        | TopCon {} -> topVersionCrash (TopCon {})
end
    
lang ConBreakerChooser = BreakerChooserInterface

    sem choose =
        | (Con {}, "use") -> { breakers = ["in"], state = Use {} }    
        | (Con {}, word) -> chooseCrash (Con {}, word)

    sem continue =
        | (Con {}, "in") -> true
        | (Con {}, word) -> false

    sem isHard =
        | (Con {}, _) -> true
    
    sem absorbIt =
        | (Con {}, "in") -> true
        | (Con {}, word) -> false

    sem topVersion =
        | Con {} -> TopCon {}

end



lang UseBreakerChooser = BreakerChooserInterface

    sem choose =
        | (Use {}, "use") -> { breakers = ["in"], state = Use {} }        
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

lang TopUtestBreakerChooser = BreakerChooserInterface
    
    sem choose =
        | (TopUtest {}, "let") -> { breakers = fullLetBreak, state = Let {} }
        | (TopUtest {}, "recursive") -> { breakers = fullLetBreak, state = Let {} }
        | (TopUtest {}, "utest") -> { breakers = fullLetBreak, state = Utest {} }

        | (TopUtest {}, "type") -> { breakers = fullTopBreak, state = Type {} }
        | (TopUtest {}, "con") -> { breakers = fullTopBreak, state = Con {} }
        | (TopUtest {}, "use") -> { breakers = ["in"], state = Use {} }    
        | (TopUtest {}, word) -> chooseCrash (TopLet {}, word)

    sem continue =
        | (TopUtest {}, _) -> true

    sem isHard =
        | (TopUtest {}, _) -> false

    sem absorbIt =
        | (TopUtest {}, word) -> false

    sem topVersion =
        | TopUtest {} -> topVersionCrash (TopUtest {})
end
    
lang UtestBreakerChooser = BreakerChooserInterface

    sem choose =
        | (Utest {}, "let") -> { breakers = fullLetBreak, state = Let {} }
        | (Utest {}, "recursive") -> { breakers = fullLetBreak, state = Let {} }
        | (Utest {}, "utest") -> { breakers = fullTopBreak, state = Utest {} }

        | (Utest {}, "type") -> { breakers = fullTopBreak, state = Type {} }
        | (Utest {}, "con") -> { breakers = fullTopBreak, state = Con {} }
        | (Utest {}, "use") -> { breakers = ["in"], state = Use {} }    
        | (Utest {}, word) -> chooseCrash (Utest {}, word)

    sem continue =
        | (Utest {}, ("in")) -> true
        | (Utest {}, _) -> false

    sem isHard =
        | (Utest {}, _) -> true
    
    sem absorbIt =
        | (Utest {}, "in") -> true
        | (Utest {}, word) -> false

    sem topVersion =
        | Utest {} -> TopUtest {}

end


    
lang BreakerChooser = ProgramBreakerChooser + TopLetBreakerChooser + LetBreakerChooser + LangBreakerChooser + TopTypeBreakerChooser + TypeBreakerChooser + SynBreakerChooser + SemBreakerChooser + ConBreakerChooser  + TopConBreakerChooser + ConBreakerChooser + MexprBreakerChooser + UseBreakerChooser + UtestBreakerChooser + TopUtestBreakerChooser end
