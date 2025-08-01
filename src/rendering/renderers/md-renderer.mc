include "./renderer-interface.mc"

lang MarkdownRenderer = RendererInterface

    sem renderTitle size s =
    | { fmt = Md {} } & opt -> 
        let size = if gti size 6 then 6 else size in
        let nl = renderNewLine opt in    
        join [repeat '#' size, " ", s, nl, nl]

    sem renderBold (text : String) =
    | { fmt = Md {} } & opt -> join ["**", text, "**"]

    sem renderNewLine =
    | { fmt = Md {} } & opt -> "  \n"

    sem renderRemoveDocForbidenChars (s: String) =
    | { fmt = Md {} } & opt ->
        switch s
        case "*" ++ r | "_" ++ r | "`" ++ r | "[" ++ r | "]" ++ r | "(" ++ r | ")" ++ r | "#" ++ r | "+" ++ r | "-" ++ r | "!" ++ r | "\\" ++ r | "<" ++ r | ">" ++ r | "`" ++ r | "{" ++ r | "}" ++ r then
             concat ['\\', head s] (renderRemoveDocForbidenChars r opt)
        case [x] ++ r then cons x (renderRemoveDocForbidenChars r opt)
        case "" then ""
        end

    sem renderRemoveCodeForbidenChars (s: String) =
    | { fmt = Md {} } & opt ->
        switch s
        case "`" ++ r then
             concat ['\\', head s] (renderRemoveCodeForbidenChars r opt)
        case [x] ++ r then cons x (renderRemoveCodeForbidenChars r opt)
        case "" then ""
        end

    sem renderDocDescription : Object -> Format -> String
    sem renderDocDescription obj =
    | { fmt = Md {} } & opt ->
        let nl = renderNewLine opt in
        let doc = objDoc obj in
        let doc = join [doc, nl, nl] in
        match splitOnR (lam c. match c with ' ' | '\n' then false else true) doc with { right = doc } in
        let doc = strReplace "\n " "\n" doc in
        if opt.mdDoc then doc else renderRemoveDocForbidenChars doc opt
    

    sem renderDocSignature (obj: Object) =
    | { fmt = Md {} } & opt ->
        let sign = renderDocSignature obj  { opt with fmt = Row { fmt = Md {} } } in
        let nl = renderNewLine opt in    
        match sign with "" then
            ""
        else
            join ["```mc\n", sign, "\n```", nl]

    sem renderGotoLink (link: String) =
    | { fmt = Md {} } & opt -> let nl = renderNewLine opt in
        join [nl, renderGotoLink link  { opt with fmt = Row { fmt = Md {} } }, nl, nl]
    
    sem renderLink (title : String) (link : String) =
    | { fmt = Md {}, urlPrefix = urlPrefix } & opt -> join ["[", title, "](", concat urlPrefix link, ")"]


    sem renderLinkList (objects: [Object]) =
    | { fmt = Md {} } & opt ->
        let nl = renderNewLine opt in
        join [renderLinkList objects { opt with fmt = Row { fmt = Md {}} }, nl]
end
