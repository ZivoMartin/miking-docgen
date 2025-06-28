-- # Token Readers Library
-- 
-- This module defines several `TokenReader` implementations, each reading a different kind of token:
-- - Weak comments ( /* ... */ style )
-- - Line comments ( -- ... )
-- - Strings ( "..." )
-- - Words (arbitrary sequences of characters)
-- - Separators (punctuation, spaces, etc.)
-- - End-of-file markers
-- - Include directives
--
-- All token readers implement a common interface: `TokenReaderInterface`.
-- The module ends with the composition of all these token readers into one combined `TokenReader`.

include "../util.mc"
include "hashmap.mc"

-- Interface definition for a generic TokenReader
lang TokenReaderInterface
    type NextResult = {token : Token, stream : String}

    -- Abstract token type to be implemented by concrete readers
    syn Token =
        
    -- Returns the original literal text of the token
    sem lit /- Token -> String -/ =
    
    -- Produces the next token from the input stream
    sem next /- : String -> NextTokenResult -/ =
    
    -- Converts the token to a human-readable string
    sem tokenToString /- Token -> String -/ =
    
    -- For debugging: print the literal
    sem display =
    | t -> print (get t)
     
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
        | "/-" ++ str  ->
            recursive
            let extract =
            lam str.
                match str with "-/" ++ xs then
                    ("", xs)                    
                else match str with [x] ++ xs then
                    let extracted = extract xs in
                    (cons x extracted.0, extracted.1)
                else
                    ("", "")
            in
            let extracted = extract str in
            {
                token = WeakComment {
                    content = extracted.0,
                    lit = concat (concat "/-" extracted.0) "-/"
                },
                stream = extracted.1
            }
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
        | "--" ++ str  ->
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
            {
                token = Comment {
                    content = extracted.0,
                    lit = concatAll ["--", extracted.0, "\n"]
                },
                stream = extracted.1
            }
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
        | "\"" ++ str  ->
            recursive
            let extract =
            lam str. lam previous.
              match str with [x] ++ xs then
                    if (and (eqc x '\"') (not (eqc previous '\\'))) then
                            ("\"", xs)
                    else
                        let extracted = extract xs x in
                        (cons x extracted.0, extracted.1)
                else ("", "")
            in
            let extracted =  extract str '-' in
            {
                token = Str {
                    content = cons '\"' extracted.0
                },
                stream = extracted.1
            }
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
    
    sem next /- : String -> NextResult -/ =
        | str ->
            match str with [x] then { token = Word { content = [x] }, stream = "" } else
            if isSep [head str] then
                { token = Word { content = [head str] }, stream = tail str }
            else let arr = [head str, head (tail str)] in if isSep arr then
                { token = Word { content = arr }, stream = tail (tail str) }
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
                {
                    token = Word { content = extracted.0 },
                    stream = extracted.1
                }
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
        | [(' ' | '\t' | '\n' ) & c] ++ str  ->
            recursive
            let extract =
            lam str.
                match str with [(' ' | '\t' | '\n' ) & x] ++ xs then
                   let extracted = extract xs in
                   (cons x extracted.0, extracted.1)
                else ("", str)
            in
            let extracted =  extract str in
            {
                token = Separator { content = cons c extracted.0 },
                stream = extracted.1
            }
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
        | ""  ->
            {
                token = Eof {},
                stream = ""
            }
end
    
-- Reader for include directives ( include "file" )
lang IncludeTokenReader = TokenReaderInterface
    syn Token =
        | Include { content: String, lit: String }

    sem lit =
        | Include { content = content, lit = lit } -> lit

    sem tokenToString =
        | Include {} -> "Include"    
    
    sem next =
        | "include " ++ str ->
            recursive
            let extractSep =
            lam str.
                match str with [(' ' | '\t' ) & x] ++ xs then
                   let extracted = extractSep xs in
                   (cons x extracted.0, extracted.1)
                else ("", str)
            in
            recursive
            let extractStr =
            lam str. lam previous.
                match str with [x] ++ xs then
                    if (and (eqc x '\"') (not (eqc previous '\\'))) then
                        ("", xs)
                    else
                        let extracted = extractStr xs x in
                        (cons x extracted.0, extracted.1)
                else
                    ("", "")
            in
            
            let extractedSep =  extractSep str in
            let extractedStr =  extractStr (tail extractedSep.1) '-' in
            {
                token = Include {
                    content = extractedStr.0,
                    lit = concatAll ["include", extractedSep.0, "\"", extractedStr.0, "\""]
                },
                stream = extractedStr.1
            }
        
end

-- Combine all token readers into a single TokenReader
lang TokenReader = StrTokenReader + CommentTokenReader + WeakCommentTokenReader + WordTokenReader + SeparatorTokenReader + EofTokenReader + IncludeTokenReader

syn Token =
    
end
