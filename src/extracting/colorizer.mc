-- # Colorizer Module
-- 
-- This module implements a **stateful syntax highlighter** for Miking source code.  
-- It transforms a stream of `Token`s into `SourceCodeWord`s, each enriched with a `SourceCodeWordKind` (e.g., keyword, name, type), and carries forward a context (`ColorizerContext`) to decide how to classify the next token.


include "../parsing/token-readers.mc"
include "./source-code-word.mc"

-- ## Common definitions used across all colorizer states
lang ColorizerInterface = SourceCodeWordKinds + TokenReader

    -- ### Enum representing the current parsing state
    syn ColorizationState =
    | Default {}       -- Regular parsing mode
    | NextIsName {}    -- After `let`, `lam`, etc., expecting a name
    | NextIsType {}    -- After `type`, `syn`, etc., expecting a type

    -- ### Context passed between tokens, includes last word and current state
    type ColorizerContext = {
        word: SourceCodeWord,             -- Previously processed word
        state: ColorizationState          -- Current FSM state
    }

    -- ### Initial context, used at the start of parsing
    sem colorizerEmptyContext : () -> ColorizerContext 
    sem colorizerEmptyContext = 
        | () -> { 
            word = buildCodeWord (Word { content = "" }) (CodeDefault {}), 
            state = Default {} 
        }

    -- ### Helper to update word color without changing state
    sem ctxChangeWord : ColorizerContext -> Token -> SourceCodeWordKind -> ColorizerContext  
    sem ctxChangeWord = 
        | { state = state } & ctx -> ctxChangeState ctx state

    -- ### Core function to update both word and state
    sem ctxChangeState : ColorizerContext -> ColorizationState -> Token -> SourceCodeWordKind  -> ColorizerContext  
    sem ctxChangeState = 
        | _ -> lam state. lam word. lam kind.  
            { word = buildCodeWord word kind, state = state }

    -- ### Signature for the main semantic function: next token → new context
    sem colorizerNext : (ColorizerContext, Token) -> ColorizerContext

end

-- ## ColorizerDefault: logic when in Default state
lang ColorizerDefault = ColorizerInterface

    sem colorizerNext =
    | ({ state = Default {} } & ctx, (Word { content = "mexpr" | "utest" | "with" | "recursive" | "match" | "end" | "switch" | "in" | "case" | "if" | "else" }) & token) 
        -> ctxChangeWord ctx token (CodeKeyword {})
    
    | ({ state = Default {} } & ctx, (Word { content = "let" | "lam" | "sem" }) & token) 
        -> ctxChangeState ctx (NextIsName {}) token (CodeKeyword {}) 
    
    | ({ state = Default {} } & ctx, (Word { content = "type" | "con" | "lang" | "syn" | "use" }) & token) 
        -> ctxChangeState ctx (NextIsType {}) token (CodeKeyword {})

    | ({ state = Default {} } & ctx, token) 
        -> ctxChangeWord ctx token (CodeDefault {})

end

-- ## ColorizerNextIsName: logic when expecting a name
lang ColorizerNextIsName = ColorizerInterface

    sem colorizerNext =
    | ({ state = NextIsName {} } & ctx, (Word { content = "mexpr" | "utest" | "with" | "recursive" | "match" | "end" | "switch" | "in" | "case" | "if" | "else" }) & token) 
        -> ctxChangeState ctx (Default {}) token (CodeKeyword {})

    | ({ state = NextIsName {} } & ctx, (Word { content = "let" | "lam" | "sem" }) & token) 
        -> ctxChangeWord ctx token (CodeKeyword {})

    | ({ state = NextIsName {} } & ctx, (Word { content = "type" | "con" | "lang" | "syn" | "use" }) & token) 
        -> ctxChangeState ctx (NextIsType {}) token (CodeKeyword {})

    | ({ state = NextIsName {} } & ctx, (Word { content = "." | "=" }) & token) 
        -> ctxChangeState ctx (Default {}) token (CodeDefault {})

    | ({ state = NextIsName {} } & ctx, (Word { content = ":" }) & token) 
        -> ctxChangeState ctx (NextIsType {}) token (CodeDefault {})

    | ({ state = NextIsName {} } & ctx, (Separator {}) & token) 
        -> ctxChangeWord ctx token (CodeDefault {})      

    | ({ state = NextIsName {} } & ctx, token) 
        -> ctxChangeState ctx (Default {}) token (CodeName {}) 
end

-- ## ColorizerNextIsType: logic when expecting a type
lang ColorizerNextIsType = ColorizerInterface

    sem colorizerNext =
    | ({ state = NextIsType {} } & ctx, (Word { content = "mexpr" | "utest" | "with" | "recursive" | "match" | "end" | "switch" | "in" | "case" | "if" | "else" }) & token) 
        -> ctxChangeState ctx (Default {}) token (CodeKeyword {})

    | ({ state = NextIsType {} } & ctx, (Word { content = "let" | "lam" | "sem" }) & token) 
        -> ctxChangeState ctx (NextIsName {}) token (CodeKeyword {})

    | ({ state = NextIsType {} } & ctx, (Word { content = "type" | "con" | "lang" | "syn" | "use" }) & token) 
        -> ctxChangeWord ctx token (CodeKeyword {})

    | ({ state = NextIsType {} } & ctx, (Word { content = "=" | "." }) & token) 
        -> ctxChangeState ctx (Default {}) token (CodeDefault {})

    | ({ state = NextIsType {} } & ctx, (Word { content = "->" }) & token) 
        -> ctxChangeWord ctx token (CodeDefault {})

    | ({ state = NextIsType {} } & ctx, (Separator {}) & token) 
        -> ctxChangeWord ctx token (CodeDefault {})

    | ({ state = NextIsType {} } & ctx, token) 
        -> ctxChangeWord ctx token (CodeType {}) 

end

-- ## Colorizer: Full colorizer composed of all state modules
lang Colorizer = ColorizerDefault + ColorizerNextIsName + ColorizerNextIsType end
