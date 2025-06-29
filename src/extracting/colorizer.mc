-- This module defines a colorize
-- It takes a stream of Object and always return SourceCode word along with context for the next word

include "../parsing/token-readers.mc"
include "./source-code-word.mc"

lang ColorizerInterface = SourceCodeWordKinds + TokenReader

    syn ColorizationState =
    | Default {}
    | NextIsName {}
    | NextIsType {}    

    type ColorizerContext = {
        word: SourceCodeWord,
        state: ColorizationState
    }

    sem colorizerEmptyContext : () -> ColorizerContext 
    sem colorizerEmptyContext = | () -> { word = buildCodeWord (Word { content = "" }) (CodeDefault {}), state = Default {} }

    sem ctxChangeWord : ColorizerContext -> Token -> SourceCodeWordKind -> ColorizerContext  
    sem ctxChangeWord = | { state = state } & ctx -> ctxChangeState ctx state
    
    sem ctxChangeState : ColorizerContext -> ColorizationState -> Token -> SourceCodeWordKind  -> ColorizerContext  
    sem ctxChangeState = | _ -> lam state. lam word. lam kind.  { word = buildCodeWord word kind, state = state }

    sem colorizerNext : (ColorizerContext, Token) -> ColorizerContext

end

lang ColorizerDefault = ColorizerInterface

    sem colorizerNext =
    | ({ word = previous, state = Default {} } & ctx, (Word { content = "let" | "lam" }) & token) -> ctxChangeState ctx (NextIsName {}) token (CodeKeyword {}) 
    | ({ word = previous, state = Default {} } & ctx, (Word { content = "type" | "con" | "lang" }) & token) -> ctxChangeState ctx (NextIsType {}) token (CodeKeyword {}) 
    | ({ word = previous, state = Default {} } & ctx, token) -> ctxChangeWord ctx token (CodeDefault {})

end


-- lang ColorizerNextIsName
    
--     sem colorizerNext =
--     | ({ word = previous, state = Default {} } & ctx, ("." | ":" | "") & token) -> ctxChangeState ctx (NextIsName {}) token (CodeKeyword {}) 

-- end

lang ColorizerNextIsType

end

    
lang Colorizer = ColorizerDefault end
