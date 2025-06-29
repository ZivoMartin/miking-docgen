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
    | ({ state = Default {} } & ctx, (Word { content = "mexpr" | "utest" | "with" | "recursive" | "match" | "end" | "switch" | "in" }) & token) -> ctxChangeWord ctx token (CodeKeyword {})
    | ({ state = Default {} } & ctx, (Word { content = "let" | "lam" | "sem" }) & token) -> ctxChangeState ctx (NextIsName {}) token (CodeKeyword {}) 
    | ({ state = Default {} } & ctx, (Word { content = "type" | "con" | "lang" | "syn" | "use" }) & token) -> ctxChangeState ctx (NextIsType {}) token (CodeKeyword {})
    
    | ({ state = Default {} } & ctx, token) -> ctxChangeWord ctx token (CodeDefault {})

end

lang ColorizerNextIsName = ColorizerInterface
    
    sem colorizerNext =
    | ({ state = NextIsName {} } & ctx, (Word { content = "mexpr" | "utest" | "with" | "recursive" | "match" | "end" | "switch" | "in" }) & token) -> ctxChangeState ctx (Default {}) token (CodeKeyword {})
    | ({ state = NextIsName {} } & ctx, (Word { content = "let" | "lam" | "sem" }) & token) -> ctxChangeWord ctx token (CodeKeyword {})
    | ({ state = NextIsName {} } & ctx, (Word { content = "type" | "con" | "lang" | "syn" | "use" }) & token) -> ctxChangeState ctx (NextIsType {}) token (CodeKeyword {})
    
    | ({ state = NextIsName {} } & ctx, (Word { content = "." | "=" }) & token) -> ctxChangeState ctx (Default {}) token (CodeDefault {})
    | ({ state = NextIsName {} } & ctx, (Word { content = ":" }) & token) -> ctxChangeState ctx (NextIsType {}) token (CodeDefault {})

    | ({ state = NextIsName {} } & ctx, (Separator {}) & token) -> ctxChangeWord ctx token (CodeDefault {})      
    | ({ state = NextIsName {} } & ctx, token) -> ctxChangeState ctx (Default {}) token (CodeName {}) -- We should normally never read another name after in the Miking syntax

end

lang ColorizerNextIsType = ColorizerInterface

    sem colorizerNext =
    | ({ state = NextIsType {} } & ctx, (Word { content = "mexpr" | "utest" | "with" | "recursive" | "match" | "end" | "switch" | "in" }) & token) -> ctxChangeState ctx (Default {}) token (CodeKeyword {})
    | ({ state = NextIsType {} } & ctx, (Word { content = "let" | "lam" | "sem" }) & token) -> ctxChangeState ctx (NextIsName {}) token (CodeKeyword {}) 
    | ({ state = NextIsType {} } & ctx, (Word { content = "type" | "con" | "lang" | "syn" | "use" }) & token) -> ctxChangeWord ctx token (CodeKeyword {})
    
    | ({ state = NextIsType {} } & ctx, (Word { content = "=" | "." }) & token) -> ctxChangeState ctx (Default {}) token (CodeDefault {})
    | ({ state = NextIsType {} } & ctx, (Word { content = "->" }) & token) -> ctxChangeWord ctx token (CodeDefault {})

    | ({ state = NextIsType {} } & ctx, (Separator {}) & token) -> ctxChangeWord ctx token (CodeDefault {})      
    | ({ state = NextIsType {} } & ctx, token) -> ctxChangeWord ctx token (CodeType {}) -- Can't go in Dafault yet because we could be in a record kind of type

end

    
lang Colorizer = ColorizerDefault + ColorizerNextIsName + ColorizerNextIsType end
