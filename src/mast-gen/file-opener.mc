include "../parsing/lexing/token-readers.mc"
include "../global/util.mc"

type ParsingFile = use TokenReader in { includes: [String], headerTokens: [{ token: Token, pos: Pos }], fileText: String }

let parsingOpenFile : String -> ParsingFile = use TokenReader in lam file.
    
    recursive let work : String -> Pos -> ParsingFile -> ParsingFile = lam s. lam pos. lam acc.
        match next s pos0 with { stream = stream, token = token, pos = pos } in
        let go = lam acc. work stream pos { acc with headerTokens = cons { token = token, pos = pos } acc.headerTokens } in
        switch token
        case Comment {} | MultiLigneComment {} | Separator {} then go acc
        case Include { content = content} then go { acc with includes = cons content acc.includes }
        case _ then { includes = reverse acc.includes, headerTokens = reverse acc.headerTokens, fileText = s }
        end
    in

    work (readOrNever file) pos0 { includes = [], headerTokens = [], fileText = "" }
    
    
    
