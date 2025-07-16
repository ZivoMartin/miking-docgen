include "../parsing/token-readers.mc"
include "./source-code-word.mc"

lang ColorizerInterface = SourceCodeWordKinds + TokenReader

    syn ColorizationState =
    | Default {}       -- Regular parsing mode
    

    type ColorizerContext = {
        word: SourceCodeWord,            
        state: ColorizationState          
    }

    sem colorizerEmptyContext : () -> ColorizerContext 
    sem colorizerEmptyContext = 
        | () -> { 
            word = buildCodeWord (Word { content = "" }) (CodeDefault {}), 
            state = Default {} 
        }

    sem ctxChangeWord : ColorizerContext -> Token -> SourceCodeWordKind -> ColorizerContext  
    sem ctxChangeWord = 
        | { state = state } & ctx -> ctxChangeState ctx state

    sem ctxChangeState : ColorizerContext -> ColorizationState -> Token -> SourceCodeWordKind  -> ColorizerContext  
    sem ctxChangeState = 
        | _ -> lam state. lam word. lam kind.  
            { word = buildCodeWord word kind, state = state }

    sem colorizerNext : (ColorizerContext, Token) -> ColorizerContext

end


lang ColorizerDefault = ColorizerInterface

    sem colorizerNext =
    | ({ state = Default {} } & ctx, token) ->
        let default = ctxChangeWord ctx token (CodeDefault {}) in
        match token with Word { content = content } then
            match content with "" then
                extractingWarn "Detected an empty word in colorizerNext";
                default
            else match content with "mexpr" | "utest" | "with" | "recursive" | "match" | "end" |
                 "switch" | "in" | "case" | "if" | "else" | "type" | "con" |
                 "lang" | "syn" | "use" | "let" | "lam" | "sem" then
                ctxChangeWord ctx token (CodeKeyword {})
            else if stringIsInt content then
                ctxChangeWord ctx token (CodeNumber {})
            else if isUpperAlpha (head content) then
                ctxChangeWord ctx token (CodeType {})
            else if isAlphaOrUnderscore (head content) then
                ctxChangeWord ctx token (CodeName {})
            else default
        else default
   

end

-- ## Colorizer: Full colorizer composed of all state modules
lang Colorizer = ColorizerDefault 
    
    sem strToSourceCode : String -> SourceCode
    sem strToSourceCode =
    | s ->
        recursive let work = lam ctx. lam s.
            match s with "" then [] else
            match next s pos0 with { token = token, stream = stream } in
            let ctx = colorizerNext (ctx, token) in
            let word = Some ctx.word in
            cons word (work ctx stream)
        in
        let ctx = colorizerEmptyContext () in
        work ctx s
        
end
