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
--     * Is this a hard break? (reStructureTree)
--     * Should this breaker be absorbed? (absorbIt)
--     * What is the top version of this state? (switchVersion)
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
        | TopRec {}
        | Rec {}
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
        | TopRec {} -> "TopRec"    
        | Rec {} -> "Rec"
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

    sem build : [String] -> State -> Breaker
    sem build =
    | breakers -> lam state. { breakers = breakers, state = state }

     
end
    
let topBreak = ["let", "recursive", "con", "lang", "sem", "syn", "type", "mexpr", "utest"]
let fullTopBreak = cons "in" topBreak

let letBreak = ["lang", "mexpr", "sem", "syn"]
let fullLetBreak = cons "in" letBreak
let fullRecBreak = ["in", "end", "lang", "mexpr", "sem", "syn"]    
    
lang ProgramBreakerChooser = BreakerChooserInterface
    
    sem choose =
        | (Program {}, "let", pos) -> build letBreak (TopLet {})
        | (Program {}, "utest", pos) -> build letBreak (TopUtest {})
        | (Program {}, "lang", pos) -> build ["end"] (Lang {})
        | (Program {}, "mexpr", pos) -> build ["lang", "mexpr"] (Mexpr {})
        | (Program {}, "type", pos) -> build topBreak (TopType {})
        | (Program {}, "con", pos) -> build topBreak (TopCon {})
        | (Program {}, "recursive", pos) -> build ["end"] (TopRec {})

    sem continue =
        | (Program {}, "") -> false

    sem absorbIt =
        | (Program {}, word) -> true

end



lang TopBreakerChooser = BreakerChooserInterface
    
    sem choose =
        | (Mexpr {} | TopLet {} | TopUtest {}, "let", pos) -> build fullLetBreak (Let {})
        | (Mexpr {} | TopLet {} | TopUtest {}, "recursive", pos) -> build fullRecBreak (TopRec {})
        | (Mexpr {} | TopLet {} | TopUtest {}, "utest", pos) -> build fullLetBreak (Utest {})
        | (Mexpr {} | TopLet {} | TopUtest {}, "type", pos) -> build fullTopBreak (Type {})
        | (Mexpr {} | TopLet {} | TopUtest {}, "con", pos) -> build fullTopBreak (Con {})
        | (Mexpr {} | TopLet {} | TopUtest {}, "use", pos) -> build ["in"] (Use {})

end

lang TopRecBreakerChooser = BreakerChooserInterface
    
    sem choose =
        | (TopRec {}, "let", pos) -> build fullLetBreak (Let {})
        | (TopRec {}, "recursive", pos) -> build fullRecBreak (Rec {})
        | (TopRec {}, "utest", pos) -> build fullLetBreak (Utest {})
        | (TopRec {}, "type", pos) -> build fullTopBreak (Type {})
        | (TopRec {}, "con", pos) -> build fullTopBreak (Con {})
        | (TopRec {}, "use", pos) -> build ["in"] (Use {})

    -- in: Downgrade et continuer le parent
    -- Lang: Downgrade ET finir le parent
    -- end: Restructurer l'arbre ET finir le parent
    
    sem continue =
        | (TopRec {}, "lang" | "mexpr" | "end") -> false

    sem reStructureTree =
        | (TopRec {}, "end") -> true

    sem absorbIt =
        | (TopRec {}, "end") -> true

    sem switchVersion =
        | (TopRec {}, "in" | "mexpr" | "lang") -> Rec {}
end


    
lang RecBreakerChooser = BreakerChooserInterface

    sem choose =
        | (Rec {}, "let", pos) -> build fullLetBreak (Let {})
        | (Rec {}, "recursive", pos) -> build fullRecBreak (Rec {})
        | (Rec {}, "utest", pos) -> build fullTopBreak (Utest {})
        | (Rec {}, "type", pos) -> build fullTopBreak (Type {})
        | (Rec {}, "con", pos) -> build fullTopBreak (Con {})
        | (Rec {}, "use", pos) -> build ["in"] (Use {})    

    sem continue =
        | (Rec {}, !"in") -> false
    
    sem absorbIt =
        | (Rec {}, "in") -> true
end
            
lang LetUtestBreakerChooser = BreakerChooserInterface

    sem choose =
        | (Let {} | Utest {}, "let", pos) -> build fullLetBreak (Let {})
        | (Let {} | Utest {}, "recursive", pos) -> build fullRecBreak (TopRec {})
        | (Let {} | Utest {}, "utest", pos) -> build fullTopBreak (Utest {})
        | (Let {} | Utest {}, "type", pos) -> build fullTopBreak (Type {})
        | (Let {} | Utest {}, "con", pos) -> build fullTopBreak (Con {})
        | (Let {} | Utest {}, "use", pos) -> build ["in"] (Use {})    

    sem continue =
        | (Let {} | Utest {}, !"in") -> false

    sem reStructureTree =
        | (Let {} | Utest {}, !"in") -> true
    
    sem absorbIt =
        | (Let {} | Utest {}, "in") -> true

    sem switchVersion =
        | (Utest {}, !"in") -> TopUtest {}
        | (Let {}, !"in") -> TopLet {}

end
    
lang TopTypeConBreakerChooser = BreakerChooserInterface
    
    sem choose =
        | (TopType {} | TopCon {}, "use", pos) -> build ["in"] (Use {})

end

lang TypeConBreakerChooser = BreakerChooserInterface

    sem choose =
        | (Type {} | Con {}, "use", pos) -> build ["in"] (Use {})

    sem continue =
        | (Type {} | Con {}, !"in") -> false

    sem reStructureTree =
        | (Type {} | Con {}, !"in") -> true

    sem absorbIt =
        | (Type {} | Con {}, "in") -> true

    sem switchVersion =
        | (Type {}, !"in") -> TopType {}
        | (Con {}, !"in") -> TopCon {}

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
        | (Lang {}, word) -> true

end

lang SemBreakerChooser = BreakerChooserInterface

    sem choose =
        | (Sem {}, "let", pos) -> build ["in"] (Let {})
        | (Sem {}, "recursive", pos) -> build ["in", "end", "syn", "sem"] (Rec {})
        | (Sem {}, "utest", pos) -> build ["in"] (Utest {})

        | (Sem {}, "type", pos) -> build langFullBreakIn (Type {})
        | (Sem {}, "con", pos) -> build langFullBreakIn (Con {})
        | (Sem {}, "syn", pos) -> build langFullBreak (Syn {})
        | (Sem {}, "sem", pos) -> build langBreak (Sem {})
        | (Sem {}, "use", pos) -> build ["in"] (Use {})        

    sem continue =
        | (Sem {}, "end") -> false

end

lang SynBreakerChooser = BreakerChooserInterface

    sem choose =
        | (Syn {}, "syn", pos) -> build langFullBreak (Syn {})
        | (Syn {}, "sem", pos) -> build langBreak (Sem {})
        | (Syn {}, "use", pos) -> build ["in"] (Use {})    

    sem continue =
        | (Syn {}, "end") -> false
    
end

lang UseBreakerChooser = BreakerChooserInterface

    sem choose =
        | (Use {}, "use", pos) -> build ["in"] (Use {})

    sem absorbIt =
        | (Use {}, "in") -> true
end


    
lang BreakerChooser = ProgramBreakerChooser + RecBreakerChooser + TopRecBreakerChooser + TopBreakerChooser + LetUtestBreakerChooser + LangBreakerChooser + TopTypeConBreakerChooser + TypeConBreakerChooser + SynBreakerChooser + SemBreakerChooser + UseBreakerChooser end
