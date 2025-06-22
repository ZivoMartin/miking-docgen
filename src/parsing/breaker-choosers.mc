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

lang BreakerChooserInterface

    type Breaker = { breakers: [String], state: State }

    
    sem chooseCrash /- (State, String) -> () -/ =
        | (state, word) -> error
                (concatAll ["You cannot have the word ", word, " inside a ", (toString state), " block."])
    sem topVersionCrash /- (State) -> () -/ =
        | state -> error (concat "You should never call topVersion for "(toString state)) 

        
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

    -- Determine the new state and the breakers after having find a block opener
    sem choose /- (State, String) -> Breaker -/ =
    -- Determine if for a given breaker, the tokenisation should continue for the parent state
    sem continue /- (State, String) -> Bool -/ =
        | (state, word) -> error (concatAll [(toString state), " ", word])
    -- Determine for a given context if the block become hard or no
    sem isHard /- (State, String) -> Bool -/ =
    -- Determine if the breaker should be part of the current block, or should remain in the stream.
    sem absorbIt /- (State, String) -> Bool -/ =
    -- TopVersion if possible the given state to it's top version, crash otherwise.
    sem topVersion /- State -> State -/ = 
end

-- TODO: let, lang, con, type, sem, syn, mexpr, recursive, include, use

lang ProgramBreakerChooser = BreakerChooserInterface

    sem choose =
        | (Program {}, "let") -> { breakers = ["lang", "in", "mexpr", "utest"], state = TopLet {} }
        | (Program {}, "recursive") -> { breakers = ["lang", "in", "mexpr", "utest"], state = TopLet {} }    
        | (Program {}, "lang") -> { breakers = ["end"], state = Lang {} }
        | (Program {}, "mexpr") -> { breakers = ["lang", "mexpr", "utest"], state = Mexpr {} }
        | (Program {}, "type") -> { breakers = ["let", "recursive", "con", "lang", "sem", "syn", "type", "mexpr", "utest"], state = TopType {} }
        | (Program {}, "con") -> { breakers = ["let", "recursive", "lang", "sem", "syn", "type", "con", "mexpr", "utest"], state = TopCon {} }
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
        | (Mexpr {}, "let") -> { breakers = ["in", "lang", "mexpr", "utest"], state = Let {} }
        | (Mexpr {}, "recursive") -> { breakers = ["in", "lang", "mexpr", "utest"], state = Let {} }    
        | (Mexpr {}, "type") -> { breakers = ["in", "let", "recursive", "lang", "type", "con", "mexpr", "utest"], state = Type {} }
        | (Mexpr {}, "con") -> { breakers = ["in", "let", "recursive", "lang", "type", "con", "mexpr", "utest"], state = Con {} }
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
        | (TopLet {}, "let") -> { breakers = ["in", "lang", "utest", "mexpr"], state = Let {} }
        | (TopLet {}, "recursive") -> { breakers = ["in", "lang", "utest", "mexpr"], state = Let {} }    
        | (TopLet {}, "type") -> { breakers = ["in", "let", "recursive", "lang", "utest", "type", "con", "mexpr"], state = Type {} }
        | (TopLet {}, "con") -> { breakers = ["in", "let", "recursive", "lang", "utest", "type", "con", "mexpr"], state = Con {} }
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
        | (Let {}, "let") -> { breakers = ["in", "lang", "utest", "mexpr"], state = Let {} }
        | (Let {}, "recursive") -> { breakers = ["in", "lang", "utest", "mexpr"], state = Let {} }    
        | (Let {}, "type") -> { breakers = ["in", "let", "recursive", "lang", "utest", "type", "sem", "syn", "end", "con", "mexpr"], state = Type {} }
        | (Let {}, "con") -> { breakers = ["in", "let", "recursive", "lang", "utest", "type", "sem", "syn", "end", "con", "mexpr"], state = Con {} }
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
        | (Type {}, "in") -> error "Unreachable, we should not ask the state machine if finding a 'in' bound to a 'type' makes the block hard."
    
    sem absorbIt =
        | (Type {}, "in") -> true
        | (Type {}, word) -> false

    sem topVersion =
        | Type {} -> TopType {}

end

        
lang LangBreakerChooser = BreakerChooserInterface

    sem choose =
        | (Lang {}, "type") -> { breakers = ["end", "sem", "syn", "type", "con"], state = TopType {} }
        | (Lang {}, "con") -> { breakers = ["end", "sem", "syn", "type", "con"], state = TopCon {} }
        | (Lang {}, "syn") -> { breakers = ["end", "sem", "syn"], state = Syn {} }
        | (Lang {}, "sem") -> { breakers = ["end", "sem", "syn"], state = Sem {} }
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
        | (Sem {}, "type") -> { breakers = ["in", "type", "syn", "sem", "end", "con"], state = Type {} }
        | (Sem {}, "con") -> { breakers = ["in", "type", "syn", "sem", "end", "con"], state = Con {} }
        | (Sem {}, "syn") -> { breakers = ["end", "sem", "syn", "type"], state = Syn {} }
        | (Sem {}, "sem") -> { breakers = ["end", "sem", "syn"], state = Sem {} }
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
        | (Syn {}, "syn") -> { breakers = ["end", "sem", "syn"], state = Syn {} }
        | (Syn {}, "sem") -> { breakers = ["end", "sem", "syn"], state = Sem {} }
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

lang BreakerChooser = ProgramBreakerChooser + TopLetBreakerChooser + LetBreakerChooser + LangBreakerChooser + TopTypeBreakerChooser + TypeBreakerChooser + SynBreakerChooser + SemBreakerChooser + ConBreakerChooser  + TopConBreakerChooser + ConBreakerChooser + MexprBreakerChooser + UseBreakerChooser end
