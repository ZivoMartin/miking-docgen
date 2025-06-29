-- This module defines a colorize
-- It takes a stream of Object and always return SourceCode word along with context for the next word

include "../parsing/token-readers.mc"
include "./source-code-word.mc"

lang ColorizerInterface

    syn ColorizationState =
    | Default {}
    | NextIsName {}
    | NextIsType {}    

    type ColorizerContext = {
        word: SourceCodeWord,
        state: ColorizationState
    }

    sem colorizerEmptyContext : () -> ColorizerContext 
    sem colorizerEmptyContext = | () -> { word = "", state = Default {} }

    sem ctxChangeWord : ColorizerContext -> SourceCodeWord -> ColorizerContext  
    sem ctxChangeWord = | { state = state } -> lam word. { word = word, state = state }

    sem ctxChangeState : ColorizerContext -> SourceCodeWord -> ColorizationState -> ColorizerContext  
    sem ctxChangeState = | _ -> lam word. lam state. { word = word, state = state }

    sem colorizerNext : (ColorizerContext, String) -> ColorizerContext

end

lang ColorizerDefault = ColorizerInterface

    sem colorizerNext =
    | ({ word = previous, state = Default {} } & ctx, ("let" | "lam") & token) -> ctxChangeState ctx token (NextIsName {})
    | ({ word = previous, state = Default {} } & ctx, ("type" | "con" | "lang") & token) -> ctxChangeState ctx token (NextIsType {})
    | ({ word = previous, state = Default {} } & ctx, token) -> ctxChangeWord ctx token

end


lang ColorizerImportantName end   

    
lang Colorizer = ColorizerDefault end
