include "util.mc"

lang TokenReaderInterface
    type NextResult = {token : Token, stream : String}

    syn Token =
        
    sem lit /- Token -> String -/ =
    
    sem next /- : String -> NextTokenResult -/ =
    
    sem tokenToString /- Token -> String -/ =
    
    sem display =
    | t -> print (get t)
     
end
    
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
                    lit = concat "--" extracted.0
                },
                stream = extracted.1
            }
end

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


lang WordTokenReader = TokenReaderInterface
    syn Token =
      | Word { content: String }

    sem lit =
        | Word { content = content } -> content


    sem tokenToString =
        | Word {} -> "Word"
    
    sem next /- : String -> NextResult -/ =
        | "=" ++ str -> { token = Word { content = "=" }, stream = str }
        | "++" ++ str -> { token = Word { content = "++" }, stream = str }
        | "|" ++ str -> { token = Word { content = "|" }, stream = str }
        | "{" ++ str -> { token = Word { content = "{" }, stream = str }
        | "}" ++ str -> { token = Word { content = "}" }, stream = str }
        | "[" ++ str -> { token = Word { content = "[" }, stream = str }
        | "]" ++ str -> { token = Word { content = "]" }, stream = str }
        | ":" ++ str -> { token = Word { content = ":" }, stream = str }
        | ";" ++ str -> { token = Word { content = ":" }, stream = str }    
        | "(" ++ str -> { token = Word { content = "(" }, stream = str }
        | ")" ++ str -> { token = Word { content = ")" }, stream = str }
        | "->" ++ str -> { token = Word { content = "->" }, stream = str }    
        | str  ->
            recursive
            let extract =
            lam str. lam previous.
                switch str 
                case (("--" ++ x) | ("++" ++ x))
                    then ("", str)                
                case [x] ++ xs then
                    match find (lam c. eqc c x) "=|{}():[];\n " with Some _ then -- Should remain valid even with ''
                        ("", str)
                    else if (and (eqc x '\"') (not (eqc previous '\\'))) then
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
    

lang IncludeTokenReader = TokenReaderInterface
    syn Token =
        | Include { content: String, lit: String }

    sem lit =
        | Include { content = content, lit = lit } -> lit

    sem tokenToString =
        | Include {} -> "Include"    
    
    sem next =
        | "include" ++ str ->
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

        
lang TokenReader = StrTokenReader + CommentTokenReader + WeakCommentTokenReader + WordTokenReader + SeparatorTokenReader + EofTokenReader + IncludeTokenReader end
