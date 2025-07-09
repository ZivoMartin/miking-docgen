include "./token-readers.mc"

type TokenStream = use TokenReader in [(Token, Pos)]
    
let lex : use TokenReader in String -> TokenStream = use TokenReader in lam stream.
    recursive let lex : String -> Pos -> TokenStream -> TokenStream = lam stream. lam pos. lam acc.
        switch next stream pos
        case { token = Eof {} & token } then reverse (cons (token, pos) acc)
        case { token = token, stream = stream, pos = newPos } then lex stream newPos (cons (token, newPos) acc)
        end
    in
    lex stream { x = 0, y = 0 } []
