-- # Token Readers Library
-- 
-- All token readers implement a common interface: `TokenReaderInterface`.
-- The module ends with the composition of all these token readers into one combined `TokenReader`.

include "../../global/util.mc"
include "../../global/logger.mc"
include "../../mast-gen/include-set.mc"
    
include "hashmap.mc"

-- Interface definition for a generic TokenReader
lang TokenReaderInterface
    
    type Pos = { x: Int, y: Int }
    type NextResult = { token : Token, stream : String, pos: Pos }
    
    -- Abstract token type to be implemented by concrete readers
    syn Token =

    -- Generate the new position in the stream from the litteral of the given token
    sem actualisePos: Pos -> Token -> Pos
    sem actualisePos =
    | pos -> lam token.
        recursive let work : String -> Pos -> Pos = lam s. lam pos.
            switch s 
            case "" then pos
            case ['\n'] ++ s then work s { x = 1, y = addi pos.y 1 }
            case ['\t'] ++ s then work s { pos with x = addi pos.x 4 }
            case [_] ++ s then work s { pos with x = addi pos.x 1 }
            end
        in work (lit token) pos

    
    -- Returns the original literal text of the token
    sem lit : Token -> String

    -- Is used by the parser to get a representation of the token.
    sem content : Token -> String
    sem content =
    | t -> lit t

    
    -- Produces the next token from the input stream
    sem next : String -> Pos -> NextResult

    -- Utility function to build a NextResult
    sem buildResult : Token -> Pos -> String -> NextResult 
    sem buildResult token pos = | stream ->  { token = token, pos = actualisePos pos token, stream = stream }
    
    -- Converts the token to a human-readable string
    sem tokenToString : Token -> String
     
end
    
-- Reader for multi lignes comments ( /* ... */ style )
lang MultiLigneCommentTokenReader = TokenReaderInterface
    syn Token =
      | MultiLigneComment { content: String, lit: String }

    sem lit =
        | MultiLigneComment { content = content, lit = lit } -> lit

    sem tokenToString =
        | MultiLigneComment {} -> "MultiLigneComment"
    
    sem next =
        | "/-" ++ str -> lam pos.
            recursive
            let extract =
            lam str. lam stack.
                switch (str, stack)
                case ("-/" ++ xs, 0) then ("", xs)
                case ("-/" ++ xs, stack) then
                    let extracted = extract xs (subi stack 1) in
                    (concat "-/" extracted.0, extracted.1)
                case ("/-" ++ xs, stack) then
                    let extracted = extract xs (addi stack 1) in
                    (concat "/-" extracted.0, extracted.1)
                case ([x] ++ xs, stack) then
                    let extracted = extract xs stack in
                    (cons x extracted.0, extracted.1)
                case _ then ("", "")
                end
            in
            let extracted = extract str 0 in
            buildResult (MultiLigneComment {
                    content = extracted.0,
                    lit = concat (concat "/-" extracted.0) "-/"
            }) pos extracted.1
end

    
-- Reader for single-line comments ( -- ... )
lang CommentTokenReader = TokenReaderInterface
    syn Token =
      | Comment { content: String, lit: String }

    sem lit =
        | Comment { lit = lit } -> lit        

    sem tokenToString =
        | Comment {} -> "Comment"
    
    sem next =
        | "--" ++ str -> lam pos.
            recursive
            let extract =
            lam str.
                match str with "\n" ++ xs then
                    ("", str)                    
                else match str with [x] ++ xs then
                    let extracted = extract xs in
                    (cons x extracted.0, extracted.1)
                else
                    ("", "")
            in
            let extracted = extract str in
            buildResult (Comment { content = extracted.0, lit = concat "--" extracted.0 }) pos extracted.1
end

-- Reader for string literals ( "..." )
lang StrTokenReader = TokenReaderInterface
    syn Token =
      | Str { content: String, between: String }

    sem lit =
        | Str { content = content } -> content

    sem tokenToString =
        | Str {} -> "Str"
    
    sem next /- : String -> NextResult -/ =
        | "\"" ++ str -> lam pos.
            recursive
            let extract =
                lam str. 
                    switch str
                    case ['\\', x] ++ xs then
                        let extracted = extract xs in
                        (concat ['\\', x] extracted.0, extracted.1)
                    case ['\"'] ++ xs then
                        ("", xs)
                    case [x] ++ xs then
                        let extracted = extract xs in
                        (cons x extracted.0, extracted.1)
                    case _ then ("", "")
                    end
                in
            let extracted =  extract str in
            buildResult (Str { content = join ["\"", extracted.0, "\""], between = extracted.0 }) pos extracted.1
end

-- Define set of separator characters / operators as a hashmap for fast lookup
let separatorMap =
    let traits = hashmapStrTraits in
    let insert = lam x. hashmapInsert traits x in
    let mem = lam x. hashmapMem traits x in
    foldl
        (lam m. lam k. hmInsert k () m)
        (hashmapEmpty ())
        ["=", "++", "|", "{", "}", "[", "]", ":", ";", ".", ",", "(", ")", "->", " ", "\n", "\t"] 

-- Predicate to check if a string is a separator
let isSep = lam s. hmMem s separatorMap

-- Reader for words (non-separator sequences)
lang WordTokenReader = TokenReaderInterface
    syn Token =
      | Word { content: String }

    sem lit =
        | Word { content = content } -> content

    sem tokenToString =
        | Word {} -> "Word"
    
    sem next =
         | str -> lam pos.
            match str with [x] then
                let token = Word { content = [x] } in
                { token = token, stream = "", pos = actualisePos pos token } else
            if isSep [head str] then
                let token = Word { content = [head str] } in
                { token = token, stream = tail str, pos = actualisePos pos token }
            else let arr = [head str, head (tail str)] in if isSep arr then
                let token = Word { content = arr } in
                { token = token, stream = tail (tail str), pos = actualisePos pos token }
            else
                recursive
                let extract =
                lam str. lam previous.
                    switch str 
                    case (("--" ++ x) | ("++" ++ x))
                        then ("", str)
                    case [x] ++ xs then
                        if isSep [x] then
                            ("", str)
                        else if and (eqc x '\"') (not (eqc previous '\\')) then
                            ("", str)
                        else
                            let extracted = extract xs x in
                            (cons x extracted.0, extracted.1)
                    case _ then ("", "")
                    end
                in
                let extracted =  extract str '-' in
                buildResult (Word { content = extracted.0 }) pos extracted.1
end

-- Reader for separators (spaces, newlines, tabs, etc.)
lang SeparatorTokenReader = TokenReaderInterface
    syn Token =
      | Separator { content: String }

    sem lit =
        | Separator { content = content } -> content

    sem tokenToString =
        | Separator {} -> "Separator"
    
    sem next =
        | [(' ' | '\t' | '\n' ) & c] ++ str -> lam pos.
            recursive
            let extract =
            lam str.
                match str with [(' ' | '\t' | '\n' ) & x] ++ xs then
                   let extracted = extract xs in
                   (cons x extracted.0, extracted.1)
                else ("", str)
            in
            let extracted =  extract str in
            buildResult (Separator { content = cons c extracted.0 }) pos extracted.1
end
    
-- Reader for End-of-File
lang EofTokenReader = TokenReaderInterface
    syn Token =
      | Eof {}

    sem tokenToString =
        | Eof {} -> "Eof"
    
    sem lit =
        | Eof {} -> ""
    
    sem next =
        | ""  -> lam pos.
            {
                token = Eof {},
                stream = "",
                pos = pos
            }
end
    
-- This lexer only support fundamental words.
lang SimpleWordTokenReader = StrTokenReader + CommentTokenReader + MultiLigneCommentTokenReader + WordTokenReader + SeparatorTokenReader + EofTokenReader end

-- Contains skip utility function allowing to jump all the comments and separator until the next important token
lang CommAndSepSkiper = SimpleWordTokenReader
    
    sem skip : String -> String -> { skiped: [Token], stream: String, newToken: Token }
    sem skip =
    | str -> lam first.
        let pos = { x = 1, y = 1 } in
        let firstSkiped = match first with "" then [] else [Separator { content = first }] in
        switch next str pos 
            case { token = (Separator {} | Comment {} | MultiLigneComment {}) & token, stream = stream } then
                let res = skip stream "" in
                let skiped = concat firstSkiped (cons token res.skiped) in
                { res with skiped = skiped }
            case { token = token, stream = stream } then { stream = stream, skiped = firstSkiped, newToken = token }
            end
end
    
-- Reader for include directives ( include "file" )
lang IncludeTokenReader = CommAndSepSkiper
    syn Token =
        | Include { content: String, lit: String, skiped: [Token] }

    sem lit =
        | Include { content = content, lit = lit } -> lit

    sem tokenToString =
        | Include {} -> "Include"

    sem includeNext =
        | str -> lam pos. lam firstSep.
            match skip str firstSep with { newToken = Str { content = str }, stream = stream, skiped = skiped } then
                let token = Include { content = subsequence str 1 (subi (length str) 2), lit = join ["include", join (map lit skiped), str], skiped = skiped } in
                buildResult token pos stream
            else
                parsingWarn "During lexing, was waiting for an Str after `include `.";
                buildResult (Word { content = concat "include" firstSep }) pos str

    sem next =
        | "include " ++ str -> lam pos. includeNext str pos " "
        | "include\n" ++ str -> lam pos. includeNext str pos "\n"
        | "include\t" ++ str -> lam pos. includeNext str pos "\t"
        | "include\"" ++ str -> lam pos. includeNext (cons '\"' str) pos ""
        | "include--" ++ str -> lam pos. includeNext (cons '\"' str) pos ""
        | "include/-" ++ str -> lam pos. includeNext (cons '\"' str) pos ""            
end

-- Specifically reads `recursive let` sequence of word
-- When `recursive` and `let` are spearated by any separator / comments
lang RecursiveTokenReader = CommAndSepSkiper
      syn Token =
      | Recursive { lit: String, skiped: [Token] }

    sem lit =
        | Recursive { lit = lit } -> lit

    sem content =
        | Recursive {} -> "recursive"

    sem tokenToString =
        | Recursive {} -> "Recursive"

end


-- This token is not readable but is at the root of a DocTree, the content is the name of the file and the includeSet a set will all the files.
lang ProgramTokenReader = TokenReaderInterface
    syn Token =
        | ProgramToken { content: String, includeSet: IncludeSet () }

    sem lit =
        | ProgramToken {} -> ""

    sem tokenToString =
        | ProgramToken {} -> "Program"
end

let pos0 = { x = 0, y = 0 }

lang ComposedWordTokenReader = RecursiveTokenReader + IncludeTokenReader + ProgramTokenReader end
        
lang RecursiveEnderTokenReader = TokenReaderInterface
     syn Token =
        | RecursiveEnderToken { ender: String }

     sem content =
        | RecursiveEnderToken { ender = ender } -> cons '#' ender

     sem lit =
        | RecursiveEnderToken { ender = ender } -> ender

    sem tokenToString =
        | RecursiveEnderToken {} -> "RecursiveEnderToken"
end

-- Combine all token readers into a single TokenReader
lang TokenReader = ComposedWordTokenReader + RecursiveEnderTokenReader end

