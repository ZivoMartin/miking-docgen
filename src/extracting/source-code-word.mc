include "../parsing/token-readers.mc"

lang SourceCodeWordKinds

    syn SourceCodeWordKind =
    | CodeKeyword {}
    | CodeName {}
    | CodeDefault {}
    | CodeType {}
    
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

let buildCodeWord : use SourceCodeWordKinds in use TokenReader in Token -> SourceCodeWordKind -> SourceCodeWord =
    use SourceCodeWordKinds in lam word. lam kind. {
        word = word,
        kind = kind    
    }       
    
