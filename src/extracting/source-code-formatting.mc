include "../parsing/lexing/token-readers.mc"
include "./source-code-word.mc"

lang FormatterInterface = SourceCodeWordKinds + TokenReader

    syn FormattingState =
    | Default {}       -- Regular parsing mode
    

    type FormatterContext = {
        word: SourceCodeWord,
        prev: String,           
        state: FormattingState          
    }

    sem formatterEmptyContext : () -> FormatterContext 
    sem formatterEmptyContext = 
        | () -> { 
            word = buildCodeWord (Word { content = "" }) (CodeDefault {}), 
            state = Default {},
            prev = ""
        }

    sem ctxChangeWord : FormatterContext -> Token -> SourceCodeWordKind -> FormatterContext  
    sem ctxChangeWord = 
        | { state = state } & ctx -> lam word. lam kind.
            let kind = switch (ctx.prev, kind)
            case (!"let", CodeName {}) then CodeDefault {}
            case _ then kind
            end in
            let prev = match word with Word { content = content } then content else ctx.prev in
            {
                word = buildCodeWord word kind,
                state = state,
                prev = prev
            }            


    sem formatterNext : (FormatterContext, Token) -> FormatterContext

end


lang FormatterDefault = FormatterInterface

    sem formatterNext =
    | ({ state = Default {} } & ctx, token) ->
        let default = ctxChangeWord ctx token (CodeDefault {}) in
        match token with Word { content = content } then
            match content with "" then
                extractingWarn "Detected an empty word in formatterNext";
                default
            else match content with "mexpr" | "utest" | "with" | "recursive" | "match" | "end" |
                 "switch" | "in" | "case" | "if" | "else" | "type" | "con" |
                 "lang" | "syn" | "use" | "let" | "lam" | "sem" | "then" then
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

-- ## Formatter: Full formatter composed of all state modules
lang Formatter = FormatterDefault 
    
    sem strToSourceCode : String -> SourceCode
    sem strToSourceCode =
    | s ->
        recursive let work = lam ctx. lam s.
            match s with "" then [] else
            match next s pos0 with { token = token, stream = stream } in
            let ctx = formatterNext (ctx, token) in
            let word = Some ctx.word in
            cons word (work ctx stream)
        in
        let ctx = formatterEmptyContext () in
        work ctx s
        
end
