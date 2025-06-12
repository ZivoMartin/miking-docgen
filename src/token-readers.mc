lang TokenReaderInterface
  type NextResult = {token : Token, stream : String}

  syn Token =

  sem lit /- Token -> String -/ =
    
  sem next /- : String -> NextTokenResult -/ =

  sem display =
    | t -> print (get t)
     
end

lang WeakCommentTokenReader = TokenReaderInterface
    syn Token =
      | WeakComment { content: String, lit: String }

    
    sem lit =
        | WeakComment { content = content, lit = lit } -> lit

        
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
    
    sem next /- : String -> NextResult -/ =
        | "\"" ++ str  ->
            recursive
            let extract =
            lam str.
                match str with "\"" ++ xs then
                    ("\"", xs)
                else match str with [x] ++ xs then
                    let extracted = extract xs in
                    (cons x extracted.0, extracted.1)
                else ("", "")
            in
            let extracted =  extract str in
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
    
    sem next /- : String -> NextResult -/ =
        | str  ->
            recursive
            let extract =
            lam str.
                match str with "--" ++ xs then
                    ("", str)
                else match str with "\"" ++ xs then
                    ("", str)
                else match str with " " ++ xs then
                    ("", str)

                else match str with [x] ++ xs then
                    let extracted = extract xs in
                    (cons x extracted.0, extracted.1)
                else ("", "")
            in
            let extracted =  extract str in
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

    sem lit =
        | Eof {} -> ""
    
    sem next =
        | ""  ->
            {
                token = Eof {},
                stream = ""
            }
end
    
    
        
lang TokenReader = StrTokenReader + CommentTokenReader + WeakCommentTokenReader + WordTokenReader + SeparatorTokenReader + EofTokenReader end
