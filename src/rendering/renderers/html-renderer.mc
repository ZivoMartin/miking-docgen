-- # HTML Renderer for mi-doc-gen
--
-- This module implements the **HtmlRenderer**, an instance of `RendererInterface`.
-- It generates HTML pages from the extracted ObjectTree.

include "./renderer-interface.mc"
include "./html-header.mc"

           
-- The HTML renderer implementation 
lang HtmlRenderer = RendererInterface

    sem renderHeader obj =
    | Html {} -> getHeader (objName obj)

    sem renderTitle size s =
    | Html {} ->
        let sizeStr = int2string (if gti size 6 then 6 else size) in
        join ["<h", sizeStr, ">", renderTitle size s (Row { fmt = Html {}}), "</h", sizeStr, ">", renderNewLine (Html {})]
    
    sem renderBold (text : String) =
    | Html {} -> join ["<strong>", text, "</strong>"]

    sem renderFooter obj =
    | Html {} -> "</div></body>\n</html>"

    sem renderNewLine =
    | Html {} -> "<br>"

    sem renderRemoveForbidenChars (s: String) =
    | Html {} ->
        switch s
        case "&" ++ s then concat "&amp;" (renderRemoveForbidenChars s (Html {}))
        case "<br>" ++ s then cons '\n' (renderRemoveForbidenChars s (Html {}))
        case "<" ++ s then concat "&lt;" (renderRemoveForbidenChars s (Html {}))
        case ">" ++ s then concat "&gt;" (renderRemoveForbidenChars s (Html {}))    
        case [x] ++ s then cons x (renderRemoveForbidenChars s (Html {}))
        case "" then ""
        end

    sem htmlRenderSpan : String -> String -> String
    sem htmlRenderSpan =
    | content -> lam kind. join ["<span class=\"", kind, "\">", content, "</span>"]

    sem renderType (content : String) = 
    | Html {} -> htmlRenderSpan content "tp"

    sem renderVar (content : String) =
    | Html {} -> htmlRenderSpan content "var"
    
    sem renderKeyword (content : String) =
    | Html {} -> htmlRenderSpan content "kw"
    
    sem renderComment (content : String) =
    | Html {} -> htmlRenderSpan content "comment"
    
    sem renderString (content : String) =
    | Html {} -> htmlRenderSpan content "string"
    
    sem renderWeakComment (content : String) =
    | Html {} -> htmlRenderSpan content "weak"

    sem renderNumber (content : String) =
    | Html {} -> htmlRenderSpan content "number"

    sem htmlRenderWrapper : all a. String -> (a -> Format -> String) -> a -> String -> String
    sem htmlRenderWrapper =
    | left -> lam f. lam arg. lam right. join [left, f arg (Row { fmt = Html {} }), right]
     
    sem renderDocBloc (data : RenderingData) =
    | Html {} ->
        htmlRenderWrapper "<div class=\"doc-block\">\n" renderDocBloc data "</div>"

    sem renderDocDescription (obj: Object) =
    | Html {} -> htmlRenderWrapper "<div class = \"doc-description\"><pre>" renderDocDescription obj "</pre></div>"

    sem renderDocSignature (obj: Object) =
    | Html {} -> htmlRenderWrapper "<div class=\"doc-signature\">" renderDocSignature obj "</div>"
    
    sem renderCodeWithoutPreview (data: RenderingData) =
    | Html {} -> htmlRenderWrapper "<div class=\"inline-container\"><pre class=\"source\">" renderCodeWithoutPreview data "</pre></div>"

    sem renderGotoLink (link: String) =
    | Html {} -> join ["<a class=\"gotoLink\" href=\"", link, "\">[→]</a>"]
    
    sem renderHidenCode (code: String) (withPreview: Bool) =
    | Html {} ->
        let jsDisplay = "<button class=\"toggle-btn\" onclick=\"toggle(this)\">...</button><div style=\"display: none;\">" in
        join [jsDisplay, if withPreview then "" else "\n", code, "</div>"]
    
    sem renderLink (title : String) (link : String) =
    | Html {} -> join ["<a href=\"", link, "\">", title, "</a>"]

    
end
