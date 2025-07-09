include "./token-readers.mc"

let lex : use TokenReader in String -> [(Token, Pos)] = use TokenReader in lam stream.
    recursive let lex : String -> Pos -> [Token] -> [Token] = lam stream. lam pos. lam acc.
        switch next stream pos
        case { token = Eof {} } then reverse acc
        case { token = token, stream = stream, pos = newPos } then lex stream newPos (cons (token, newPos) acc)
        end
    in
    lex stream { x = 0, y = 0 } []
