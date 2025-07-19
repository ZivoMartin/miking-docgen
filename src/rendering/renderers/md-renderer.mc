include "./renderer-interface.mc"

lang MarkdownRenderer = RendererInterface

    sem renderTitle size s =
    | Md {} -> 
        let size = if gti size 6 then 6 else size in
        let nl = renderNewLine (Md {}) in    
        join [repeat '#' size, " ", s, nl, nl]

    sem renderBold (text : String) =
    | Md {} -> join ["**", text, "**"]

    sem renderNewLine =
    | Md {} -> "  \n"

    sem renderRemoveForbidenChars (s: String) =
    | Md {} ->
        switch s
        case "*" ++ r | "_" ++ r | "`" ++ r | "[" ++ r | "]" ++ r | "(" ++ r | ")" ++ r | "#" ++ r | "+" ++ r | "-" ++ r | "!" ++ r | "\\" ++ r then
             concat ['\\', head s] (renderRemoveForbidenChars r (Md {}))
        case [x] ++ r then cons x (renderRemoveForbidenChars r (Md {}))
        case "" then ""
        end

    sem renderDocDescription : Object -> Format -> String
    sem renderDocDescription obj =
    | Md {} ->
        let nl = renderNewLine (Md {}) in
        let doc = objDoc obj in
        let doc = join [doc, nl, nl] in
        match splitOnR (lam c. match c with ' ' | '\n' then false else true) doc with { right = doc } in
        strReplace "\n " "\n" doc
        
    

    sem renderDocSignature (obj: Object) =
    | Md {} ->
        let sign = renderDocSignature obj (Row { fmt = Md {} }) in
        let nl = renderNewLine (Md {}) in    
        match sign with "" then
            ""
        else
            join ["```", sign, "```", nl]

    sem renderGotoLink (link: String) =
    | Md {} -> let nl = renderNewLine (Md {}) in
    join [nl, renderGotoLink link (Row { fmt = Md {} }), nl, nl]
    
    sem renderLink (title : String) (link : String) =
    | Md {} -> join ["[", title, "](", link, ")"]


    sem renderLinkList (objects: [Object]) =
    | Md {} ->
        let nl = renderNewLine (Md {}) in
        join [renderLinkList objects (Row { fmt = Md {}}), nl]
end
