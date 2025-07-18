include "./renderer-interface.mc"

lang MarkdownRenderer = RendererInterface

    sem renderTitle size s =
    | Md {} -> 
        let size = if gti size 6 then 6 else size in
        join [repeat '#' size, " ", s, "\n"]

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
        let doc = objDoc obj in
        match splitOnR (lam c. match c with ' ' | '\n' then false else true) doc with { right = doc } in
        strReplace "\n " "\n" doc
        
    

    sem renderDocSignature (obj: Object) =
    | Md {} -> join ["```", renderDocSignature obj (Row { fmt = Md {} }), "```"]

    sem renderLink (title : String) (link : String) =
    | Md {} -> join ["[", title, "](", link, ")"]
    
end
