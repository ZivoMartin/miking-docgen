-- # HTML Renderer for mi-doc-gen
--
-- This module implements the **HtmlRenderer**, an instance of `RendererInterface`.
-- It generates HTML pages from the extracted ObjectTree.

include "./renderer-interface.mc"
include "./headers/html-themes.mc"

           
-- The HTML renderer implementation 
lang HtmlRenderer = RendererInterface

    sem renderHeader obj =
    | { fmt = Html {}, theme = theme } & opt -> getHeader theme (objName obj)

    sem renderTitle size s =
    | { fmt = Html {} } & opt ->
        let sizeStr = int2string (if gti size 6 then 6 else size) in
        join ["<h", sizeStr, ">", renderTitle size s { opt with fmt = Row { fmt = Html {}} }, "</h", sizeStr, ">", renderNewLine opt]
    
    sem renderBold (text : String) =
    | { fmt = Html {} } & opt -> join ["<strong>", text, "</strong>"]

    sem renderFooter obj =
    | { fmt = Html {} } & opt -> "</div></body>\n</html>"   

    sem renderNewLine =
    | { fmt = Html {} } & opt -> "<br>"

    sem renderRemoveDocForbidenChars (s: String) =
    | { fmt = Html {} } & opt ->
        switch s
        case "&" ++ s then concat "&amp;" (renderRemoveDocForbidenChars s opt)
        case "<br>" ++ s then cons '\n' (renderRemoveDocForbidenChars s opt)
        case "<" ++ s then concat "&lt;" (renderRemoveDocForbidenChars s opt)
        case ">" ++ s then concat "&gt;" (renderRemoveDocForbidenChars s opt)    
        case [x] ++ s then cons x (renderRemoveDocForbidenChars s opt)
        case "" then ""
        end

    sem renderRemoveCodeForbidenChars (s: String) =
    | { fmt = Html {} } & opt -> renderRemoveDocForbidenChars s opt

    sem htmlRenderSpan : String -> String -> String
    sem htmlRenderSpan =
    | content -> lam kind. join ["<span class=\"", kind, "\">", content, "</span>"]

    sem renderType (content : String) = 
    | { fmt = Html {} } & opt -> htmlRenderSpan content "tp"

    sem renderVar (content : String) =
    | { fmt = Html {} } & opt -> htmlRenderSpan content "var"
    
    sem renderKeyword (content : String) =
    | { fmt = Html {} } & opt -> htmlRenderSpan content "kw"
    
    sem renderComment (content : String) =
    | { fmt = Html {} } & opt -> htmlRenderSpan content "comment"
    
    sem renderString (content : String) =
    | { fmt = Html {} } & opt -> htmlRenderSpan content "string"
    
    sem renderMultiLigneComment (content : String) =
    | { fmt = Html {} } & opt -> htmlRenderSpan content "multi"

    sem renderNumber (content : String) =
    | { fmt = Html {} } & opt -> htmlRenderSpan content "number"

    sem htmlRenderWrapper : all a. RenderingOptions -> String -> (a -> RenderingOptions -> String) -> a -> String -> String
    sem htmlRenderWrapper =
    | opt -> lam left. lam f. lam arg. lam right.
        let inner = f arg { opt with fmt = Row { fmt = Html {} } } in
        match inner with "" then "" else join [left, inner, right]

    sem renderTopPageDoc (data: RenderingData) =
    | { fmt = Html {} } & opt -> htmlRenderWrapper opt "<div class=\"top-doc\">\n<pre>" renderTopPageDoc data "</pre>\n</div>"    
    
    sem renderDocBloc (data : RenderingData) =
    | { fmt = Html {} } & opt -> htmlRenderWrapper opt "<div class=\"doc-block\">\n<pre>" renderDocBloc data "</pre>\n</div>"

    sem renderDocDescription (obj: Object) =
    | { fmt = Html {} } & opt -> htmlRenderWrapper opt "<div class = \"doc-description\"><pre>" renderDocDescription obj "</pre></div>"

    sem renderDocSignature (obj: Object) =
    | { fmt = Html {} } & opt -> htmlRenderWrapper opt "<div class=\"doc-signature\">" renderDocSignature obj "</div>"
    
    sem renderCodeWithoutPreview (data: RenderingData) =
    | { fmt = Html {} } & opt -> htmlRenderWrapper opt "<div class=\"code-block\"><pre>" renderCodeWithoutPreview data "</pre></div>"

    sem renderGotoLink (link: String) =
    | { fmt = Html {} } & opt -> join ["<a class=\"gotoLink\" href=\"", link, "\">[→]</a>"]
    
    sem renderHidenCode (code: String) (withPreview: Bool) =
    | { fmt = Html {} } & opt ->
        let jsDisplay = "<button class=\"toggle-btn\" onclick=\"toggle(this)\">...</button><div class=\"hiden-code\" style=\"display: none;\">" in
        join [jsDisplay, if withPreview then "" else "\n", code, "</div>"]
    
    sem renderLink (title : String) (link : String) =
    | { fmt = Html {}, urlPrefix = urlPrefix } & opt -> join ["<a href=\"", concat urlPrefix link, "\">", title, "</a>"]

    
end
