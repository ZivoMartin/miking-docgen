-- # HTML Renderer for mi-doc-gen
--
-- This module implements the **HtmlRenderer**, an instance of `RendererInterface`.
-- It generates HTML pages from the extracted ObjectTree.

include "../renderer-interface.mc"
include "./header.mc"

let htmlBalise = lam s. lam b. join ["<", b, ">\n", s, "\n</", b, ">"]

let htmlSpan = lam content. lam kind. join ["<span class=\"", kind, "\">", content, "</span>"]
           
-- The HTML renderer implementation 
lang HtmlRenderer = RendererInterface

    sem renderHeader obj =
    | Html {} -> getHeader (objName obj)

    sem renderTitle size s =
    | Html {} ->
        let sizeStr = int2string (if gti size 6 then 6 else size) in
        join ["<h", sizeStr, ">", renderTitle size s (Row { fmt = Html {}}), "</h", sizeStr, ">", renderNewLine (Html {})]
    
    sem renderBold (text : String) =
    | Html {} -> htmlBalise text "strong"

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


    sem renderType (content : String) = 
    | Html {} -> htmlSpan content "tp"

    sem renderVar (content : String) =
    | Html {} -> htmlSpan content "var"
    
    sem renderKeyword (content : String) =
    | Html {} -> htmlSpan content "kw"
    
    sem renderComment (content : String) =
    | Html {} -> htmlSpan content "comment"
    
    sem renderString (content : String) =
    | Html {} -> htmlSpan content "st"
    
    sem renderWeakComment (content : String) =
    | Html {} -> htmlSpan content "comment"

    sem renderRenderingData (data : RenderingData) =
    | Html {} ->
        join ["<div class=\"ObjectParent\">\n",
            renderRenderingData data (Row { fmt = Html {} }),
            "</div>"]

    sem renderDoc (doc: String) =
    | Html {} -> join ["<pre class=md>", renderDoc doc (Row { fmt = Html {} }), "</pre>"]

    sem renderLabel (label: String) =
    | Html {} -> join ["<pre class=code>", label, "</pre>"]

    sem renderGotoLink (link: String) =
    | Html {} -> join ["<a class=\"gotoLink\" href=\"", link, "\">[→]</a>"]
    
    sem renderHidenCode (code: String) =
    | Html {} ->
        let jsDisplay = "<button class=\"toggle-btn\" onclick=\"toggle(this)\">...</button><div style=\"display: none;\">" in
        join [jsDisplay, renderNewLine (Html {}),  "</div>"]

    sem renderCodeWithPreview (data: RenderingData) =
    | Html {} ->
         join ["<div class=\"inline-container\"><pre class=\"source\">", renderCodeWithoutPreview data (Row { fmt = Html {}}), "</pre></div>"]

    sem renderLink (title : String) (link : String) =
    | Html {} -> join ["<a href=\"", link, "\">", title, "</a>"]
    
end
