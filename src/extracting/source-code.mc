include "./source-code-word.mc"

-- A linear buffer of words where `None` denotes a child-boundary placeholder.
type SourceCode = [SourceCodeWord]

let tokensToSourceCode : use TokenReader in [Token] -> SourceCode = map sourceCodeWordFormat

-- Cast a string to a SourceCode by tokenizing the string until eof.
recursive let strToSourceCode : String -> SourceCode = use TokenReader in lam s.
    match s with "" then [] else
    match next s pos0 with { token = token, stream = stream } in
    let word = sourceCodeWordFormat token in
    cons word (strToSourceCode stream)
end

let sourceCodeIsEmpty : SourceCode -> Bool = null

let sourceCodeEmpty : () -> SourceCode = lam . []
