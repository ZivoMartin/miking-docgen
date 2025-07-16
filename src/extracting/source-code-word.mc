-- # SourceCodeWord Module
--
-- This module defines the data structure used by the colorizer to represent a **highlighted token** from the source code.
-- It associates a `Token` with a `SourceCodeWordKind`, which indicates how the token should be rendered (e.g., as a keyword, type, name, etc.).

include "../parsing/token-readers.mc"

-- ## SourceCodeWordKinds
--
-- Enumerates all supported visual categories for a token.
-- These determine how tokens are rendered or classified by the colorizer.
lang SourceCodeWordKinds

    syn SourceCodeWordKind =
    | CodeKeyword {}
    | CodeName {}
    | CodeDefault {}
    | CodeType {}
    | CodeNumber {}
    
end

-- ## SourceCodeWord
--
-- Represents a single word from the source code.
-- - `Some word` -> an actual token string
-- - `None {}`   -> a placeholder for a child block's source code
type SourceCodeWord = use SourceCodeWordKinds in use TokenReader in  {
    word: Token,
    kind: SourceCodeWordKind
}

-- A utility function to create a `SourceCodeWord` from a `Token` and a `SourceCodeWordKind`.
let buildCodeWord : use SourceCodeWordKinds in use TokenReader in Token -> SourceCodeWordKind -> SourceCodeWord =
    use SourceCodeWordKinds in lam word. lam kind. {
        word = word,
        kind = kind    
    }       
    
