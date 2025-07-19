include "./renderer-interface.mc"

lang MdxRenderer = RendererInterface

    sem renderTitle size s =
    | Mdx {} -> renderTitle size s (Md {})

    sem renderBold (text : String) =
    | Mdx {} -> renderBold text (Md {})

    sem renderNewLine =
    | Mdx {} -> renderTitle (Md {})

    sem renderRemoveForbidenChars (s: String) =
    | Mdx {} -> renderRemoveForbidenChars s (Md {})

    sem renderDocDescription obj =
    | Mdx {} -> renderDocDescription obj (Md {})
        
    sem renderDocSignature (obj: Object) =
    | Mdx {}  -> renderDocSignature size s (Md {})

    sem renderGotoLink (link: String) =
    | Mdx {} -> renderGotoLink link (Md {})
    
    sem renderLink (title : String) (link : String) =
    | Mdx {} -> renderLink title (Md {})


    sem renderLinkList (objects: [Object]) =
    | Mdx {} -> renderLinkList objects (Md {})
    
end
