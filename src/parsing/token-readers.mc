-- # Token Readers Library
-- 
-- All token readers implement a common interface: `TokenReaderInterface`.
-- The module ends with the composition of all these token readers into one combined `TokenReader`.

include "../util.mc"
include "../logger.mc"
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
    
-- Reader for weak comments ( /* ... */ style )
lang WeakCommentTokenReader = TokenReaderInterface
    syn Token =
      | WeakComment { content: String, lit: String }

    sem lit =
        | WeakComment { content = content, lit = lit } -> lit

    sem tokenToString =
        | WeakComment {} -> "WeakComment"
    
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
            buildResult (WeakComment {
                    content = extracted.0,
                    lit = concat (concat "/-" extracted.0) "-/"
            }) pos extracted.1
end

    
-- Reader for single-line comments ( -- ... )
lang CommentTokenReader = TokenReaderInterface
    syn Token =
      | Comment { content: String, lit: String }

    sem lit =
        | Comment { content = content, lit = lit } -> lit        

    sem tokenToString =
        | Comment {} -> "Comment"
    
    sem next =
        | "--" ++ str -> lam pos.
            recursive
            let extract =
            lam str.
                match str with "\n" ++ xs then
                    ("", xs)                    
                else match str with [x] ++ xs then
                    let extracted = extract xs in
                    (cons x extracted.0, extracted.1)
                else
                    ("", "")
            in
            let extracted = extract str in
            buildResult (Comment { content = extracted.0, lit = concatAll ["--", extracted.0, "\n"] }) pos extracted.1
end

-- Reader for string literals ( "..." )
lang StrTokenReader = TokenReaderInterface
    syn Token =
      | Str { content: String }

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
                        ("\"", xs)
                    case [x] ++ xs then
                        let extracted = extract xs in
                        (cons x extracted.0, extracted.1)
                    case _ then ("", "")
                    end
                in
            let extracted =  extract str in
            buildResult (Str { content = cons '\"' extracted.0 }) pos extracted.1
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

-- This token is not readable but is at the root of a DocTree, the content is the name of the file.
lang ProgramTokenReader = TokenReaderInterface
    syn Token =
        | ProgramToken { content: String }

    sem lit =
        | ProgramToken {} -> ""

    sem tokenToString =
        | ProgramToken {} -> "Program"
end
    
-- This lexer only support fundamental words.
lang SimpleWordTokenReader = StrTokenReader + CommentTokenReader + WeakCommentTokenReader + WordTokenReader + SeparatorTokenReader + EofTokenReader + ProgramTokenReader end

-- Contains skip utility function allowing to jump all the comments and separator until the next important token
lang CommAndSepSkiper = SimpleWordTokenReader
    
    sem skip : String -> String -> { skiped: [Token], stream: String, newToken: Token }
    sem skip =
    | str -> lam first.
        let pos = { x = 1, y = 1 } in
        let firstSkiped = match first with "" then [] else [Separator { content = first }] in
        switch next str pos 
            case { token = (Separator {} | Comment {} | WeakComment {}) & token, stream = stream } then
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
                let token = Include { content = subsequence str 1 (subi (length str) 2), lit = concatAll ["include", concatAll (map lit skiped), str], skiped = skiped } in
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


    sem recNext =
    | str -> lam pos. lam firstSep.
        match skip str firstSep with { newToken = Word { content = "let" }, stream = stream, skiped = skiped } then
            let token = Recursive { lit = concatAll ["recursive", concatAll (map lit skiped), "let"], skiped = skiped } in
            buildResult token pos stream
        else
            parsingWarn "During lexing, was waiting for a let word after `recursive`.";
            buildResult (Word { content = concat "recursive" firstSep }) pos str

    sem next =
        | "recursive " ++ str -> lam pos. recNext str pos " "
        | "recursive\n" ++ str -> lam pos. recNext str pos "\n"
        | "recursive\t" ++ str -> lam pos. recNext str pos "\t"
        | "recursive--" ++ str -> lam pos. recNext str pos ""
        | "recursive-/" ++ str -> lam pos. recNext str pos ""            
end


lang ComposedWordTokenReader = RecursiveTokenReader + IncludeTokenReader end
        
-- Combine all token readers into a single TokenReader
lang TokenReader = ComposedWordTokenReader end


mexpr use TokenReader in

let pos0 = { x = 1, y = 1 } in

-- WeakComment tests
utest (next "/- hello -/" pos0).token with WeakComment { lit = "/- hello -/", content = " hello "} in
utest (next "/- /- nested -/ -/" pos0).token with WeakComment { lit = "/- /- nested -/ -/", content = " /- nested -/ "} in

-- Comment tests
utest (next "-- a comment\n" pos0).token with Comment { lit = "-- a comment\n", content = " a comment"} in
utest (next "--\n" pos0).token with Comment { lit = "--\n", content = ""} in

-- String tests
utest (next "\"hello world\"" pos0).token with Str { content = "\"hello world\""} in
utest (next "\"escaped \\\" quote\"" pos0).token with Str { content = "\"escaped \\\" quote\""} in

-- Word tests
utest next "hello(" pos0 with { token = Word { content = "hello"}, stream = "(", pos = { x = 5, y = 1 } } in
utest (next "x1_y2" pos0).token with Word { content = "x1_y2" } in
utest (next "includeX" pos0).token with Word { content = "includeX" } in
utest (next "recursivelet" pos0).token with Word { content = "recursivelet" } in    

-- Separator tests
utest (next " " pos0).token with Separator { content = " " } in
utest (next "\n" pos0).token with Separator { content = "\n" } in
utest (next "\t" pos0).token with Separator { content = "\t" } in
utest (next "(" pos0).token with Word { content = "(" } in
utest (next "++" pos0).token with Word { content = "++" } in
utest (next "->" pos0).token with Word { content = "->" } in

-- EOF test
utest (next "" pos0).token with Eof {} in

-- Include tests
utest (next "include \"file.mc\"" pos0).token with Include { lit = "include \"file.mc\"", content = "file.mc", skiped = [Separator { content = " "}] } in
utest (next "include\t\"lib.miking\"" pos0).token with Include { lit = "include\t\"lib.miking\"", content = "lib.miking", skiped = [Separator { content = "\t"}] } in
utest (next "include\n\"abc\"" pos0).token with Include { lit = "include\n\"abc\"", content = "abc", skiped = [Separator { content = "\n"}] } in

-- Recursive tests
utest (next "recursive /- hello -/ let" pos0).token with Recursive { lit = "recursive /- hello -/ let", skiped = [Separator { content = " "}, WeakComment { content = " hello ", lit = "/- hello -/" }, Separator { content = " " }] } in
utest (next "recursive \n\t let" pos0).token with Recursive { lit = "recursive \n\t let", skiped = [Separator { content = " " }, Separator { content = "\n\t "}] } in

-- Complex mixed test
utest (next "/- a comment -/ include \"x.mc\"" pos0).token with WeakComment { lit = "/- a comment -/", content = " a comment " } in


-- Position after a simple word
utest (next "hello" pos0).pos with { x = 5, y = 1 } in

-- Position after weak comment
utest (next "/- hi -/" pos0).pos with { x = 8, y = 1 } in

-- Position after line comment with newline
utest (next "-- line\n" pos0).pos with { x = 1, y = 1 } in

-- Position after multi-line separator
utest (next "   \n\t" pos0).pos with { x = 4, y = 1 } in  -- space(3) + \n (y++) + tab(x+=4)

-- Position after string
utest (next "\"abc\"" pos0).pos with { x = 5, y = 1 } in

-- Position after include "lib.mc"
utest (next "include \"lib.mc\"" pos0).pos with { x = 16, y = 1 } in  -- include + space + quoted string

-- Position after recursive let
utest (next "recursive let" pos0).pos with { x = 13, y = 1 } in

-- String with newline inside — should be tokenized as one line (not split)
utest (next "\"line1\\nline2\"" pos0).pos with { x = 14, y = 1 } in

-- Separator with multiple newlines
utest (next "\n\n" pos0).pos with { x = 1, y = 2 } in

-- Word stopped by separator
utest (next "abc;" pos0).token with Word { content = "abc" } in
utest (next "abc;" pos0).pos with { x = 3, y = 1 } in

-- Include directive with newline before quote
utest (next "include\n\"x\"" pos0).token with Include { lit = "include\n\"x\"", content = "x", skiped = [ Separator { content = "\n" } ] } in
utest (next "include\n\"x\"" pos0).pos with { x = 3, y = 1 } in

-- Recursive let with newline and tab
utest (next "recursive\n\tlet" pos0).pos with { x = 7, y = 1 } in 

-- EOF position should remain unchanged
utest (next "" pos0).pos with { x = 1, y = 1 } in

-- WeakComment
utest (next "/- a -/next" pos0).stream with "next" in

-- Line comment
utest (next "-- line comment\nnext" pos0).stream with "next" in

-- String
utest (next "\"hello\" rest" pos0).stream with " rest" in

-- Word
utest (next "word next" pos0).stream with " next" in

-- Separator (single space)
utest (next " \nnext" pos0).stream with "next" in

-- Separator (multiple spaces and tabs)
utest (next " \t  \nnext" pos0).stream with "next" in

-- Eof (empty input)
utest (next "" pos0).stream with "" in

-- Include directive
utest (next "include \"file.mc\" next" pos0).stream with " next" in

-- Recursive let
utest (next "recursive \t\nlet next" pos0).stream with " next" in

-- Word followed by separator
utest (next "abc;" pos0).stream with ";" in

-- Separator followed by word
utest (next " \tfoo" pos0).stream with "foo" in

-- String followed by separator
utest (next "\"a\";" pos0).stream with ";" in

-- Comment followed by string
utest (next "-- comment\n\"string\"" pos0).stream with "\"string\"" in

-- Weak comment followed by recursive let
utest (next "/- c -/recursive let" pos0).stream with "recursive let" in

-- Include with newline then word
utest (next "include\n\"lib\"\nmain" pos0).stream with "\nmain" in

      
()
