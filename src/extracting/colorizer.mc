-- This module defines a colorize
-- It takes a stream of Object and always return SourceCode word along with context for the next word

include "../parsing/token-readers.mc"
include "./source-code-word.mc"

lang ColorizerInterface = TokenReader

    syn ColorizationState =
    | Default {}
    | ImportantName {}

    type ColorizerContext = {
        word: SourceCodeWord,
        state: ColorizationState
    }

    sem next : (ColorizerContext, Token) -> ColorizerContext

end

lang ColorizerDefault


end


lang ColorizerImportantName end   

    
lang Colorizer end
