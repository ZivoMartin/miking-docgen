lang TokenReaderInterface
  type NextResult = {token : Token, stream : String}

  syn Token =
    
  sem next /- : String -> NextTokenResult -/ =

  sem display /- : Token -> () -/ =
end

lang WeakCommentTokenReader = TokenReaderInterface
    syn Token =
      | WeakComment { comment: String }

    sem display =
        | WeakComment { comment = content } ->
            print "/-";
            print content;
            print "-/"
        
        
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
                token = WeakComment { comment = extracted.0 },
                stream = extracted.1
            }
            
end

    

lang CommentTokenReader = TokenReaderInterface
    syn Token =
      | Comment { comment: String }

    sem display =
        | Comment { comment = content } ->
            print "--";
            print content;
            print "\n"
        
        
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
                token = Comment { comment = extracted.0 },
                stream = extracted.1
            }
            
end

lang StrTokenReader = TokenReaderInterface
    syn Token =
      | Str { content: String }

    sem display =
        | Str { content = content } ->
            print "\"";
            print content;
            print "\""
    
    sem next /- : String -> NextResult -/ =
        | "\"" ++ str  ->
            recursive
            let extract =
            lam str.
                match str with "\"" ++ xs then
                    ("", xs)
                else match str with [x] ++ xs then
                    let extracted = extract xs in
                    (cons x extracted.0, extracted.1)
                else ("", "")
            in
            let extracted =  extract str in
            {
                token = Str { content = extracted.0 },
                stream = extracted.1
            }
end


lang WordTokenReader = TokenReaderInterface
    syn Token =
      | Word { content: String, breaker: String }

    sem display =
        | Word { content = content, breaker = breaker } ->
            print content;
            print breaker
    
    sem next /- : String -> NextResult -/ =
        | str  ->
            recursive
            let extract =
            lam str.
                match str with "--" ++ xs then
                    ("", "", str)
                else match str with "\"" ++ xs then
                    ("", "", str)
                else match str with " " ++ xs then
                    ("", " ", xs)

                else match str with [x] ++ xs then
                    let extracted = extract xs in
                    (cons x extracted.0, extracted.1, extracted.2)
                else ("", "", "")
            in
            let extracted =  extract str in
            {
                token = Word { content = extracted.0, breaker = extracted.1 },
                stream = extracted.2
            }
end
        
lang TokenReader = StrTokenReader + CommentTokenReader + WeakCommentTokenReader + WordTokenReader end
