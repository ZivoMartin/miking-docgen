include "./renderer-interface.mc"
include "./headers/mdx-header.mc"

lang MdxRenderer = RendererInterface

    sem renderHeader obj =
    | { fmt = Mdx {} } -> mdxHeader
     
    sem renderTitle size s =
    | { fmt = Mdx {} } & opt -> renderTitle size s { opt with fmt = Md {} }

    sem renderBold (text : String) =
    | { fmt = Mdx {} } & opt -> renderBold text { opt with fmt = Md {} }

    sem renderNewLine =
    | { fmt = Mdx {} } & opt -> renderNewLine { opt with fmt = Md {} }

    sem renderRemoveForbidenChars (s: String) =
    | { fmt = Mdx {} } & opt -> renderRemoveForbidenChars s { opt with fmt = Md {} }

    sem renderDocDescription obj =
    | { fmt = Mdx {} } & opt -> renderDocDescription obj { opt with fmt = Md {} }
        
    sem renderGotoLink (link: String) =
    | { fmt = Mdx {} } & opt -> renderGotoLink link { opt with fmt = Md {} }
    
    sem renderLink (title : String) (link : String) =
    | { fmt = Mdx {} } & opt -> renderLink title link { opt with fmt = Md {} }
    
    sem renderLinkList (objects: [Object]) =
    | { fmt = Mdx {} } & opt -> renderLinkList objects { opt with fmt = Md {} }

    sem renderDocSignature (obj: Object) =
    | { fmt = Mdx {} } & opt ->
        let sign = renderDocSignature obj  { opt with fmt = Row { fmt = Mdx {} } } in
        let nl = renderNewLine opt in    
        match sign with "" then
            ""
        else
            join ["\n```mc\n", sign, "\n```\n"]

    sem renderHidenCode (code: String) (withPreview: Bool) =
    | { fmt = Mdx {} } -> join ["\n<ToggleWrapper>\n```mc\n", code, "\n```\n</ToggleWrapper>\n"]

end
