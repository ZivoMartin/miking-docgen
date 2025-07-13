-- # Lexer
--
-- This module defines the `lex` function, which tokenizes a Miking source string.
-- It produces a `TokenStream`, which is a list of `(Token, Pos)` pairs.
-- The lexer uses the `next` function from `token-readers.mc` to scan the input string
-- and records the position of each token.

include "./token-readers.mc"

-- A list of tokens paired with their position in the source file.
type TokenStream = use TokenReader in [(Token, Pos)]
    
-- Lexical analysis: converts a raw source string into a stream of (token, position) pairs.
-- The lexer uses a recursive helper that scans the input until EOF is reached.
let lex : use TokenReader in String -> TokenStream = use TokenReader in lam stream.
    recursive let lex : String -> Pos -> TokenStream -> TokenStream = lam stream. lam pos. lam acc.
        switch next stream pos
        case { token = Eof {} & token } then reverse (cons (token, pos) acc)
        case { token = token, stream = stream, pos = newPos } then lex stream newPos (cons (token, newPos) acc)
        end
    in
    lex stream { x = 0, y = 0 } []
