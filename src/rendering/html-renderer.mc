-- # HTML Renderer for mi-doc-gen
--
-- This module implements the **HtmlRenderer**, an instance of `RendererInterface`.
-- It generates HTML pages from the extracted ObjectTree.

include "../extracting/source-code-word.mc"
include "renderer-interface.mc"
include "../extracting/objects.mc"    
include "../extracting/source-code-reconstruction.mc"
include "./source-code-spliter.mc"
include "./html-header.mc"

-- HTML helpers
let htmlBalise = lam s. lam b. concatAll ["<", b, ">\n", s, "\n</", b, ">"]
let htmlText = lam s. htmlBalise s "p"
let htmlPre = lam s. concatAll ["<pre>", s, "</pre>"]
let htmlCode = lam s. concatAll ["<pre class=code>", s, "</pre>"]
let htmlStrong = lam s. htmlText (htmlBalise s "strong")

let htmlGetLink = lam l. lam txt. concatAll ["<a href=\"/", l, "\">", txt, "</a>"]
let htmlGetLangLink = lam lng. htmlGetLink (concat (getLangLink lng) ".lang") lng

let htmlDoc = lam doc. concatAll ["<pre class=md>", doc, "</pre>"]

let htmlBuildCodeSource : Object -> [ObjectTree] -> String = use SourceCodeWordKinds in use TokenReader in lam obj: Object. lam sons: [ObjectTree].

    let getHidenCode = lam code.
        let jsDisplay = "<button class=\"toggle-btn\" onclick=\"(function(btn){ const div = btn.nextElementSibling; if (div.style.display === 'none') { div.style.display = 'inline'; } else { div.style.display = 'none'; } })(this)\">...</button><div style=\"display: none;\">" in
        concatAll [jsDisplay, code, "</div>"] in

    let getColorizedSnippet = lam buffer.
        concatAll (map (lam w.
        switch w
        case { word = Include { content = content } } then
            concatAll ["<span class=\"kw\">include</span> <span class=\"string\">\"", content, "\"</span>"]
        case { word = word, kind = kind } then
            let class = (switch kind
            case CodeKeyword {} then "kw"
            case CodeName {} then "var"
            case CodeType {} then "tp"
            case CodeDefault {} then
                switch word
                case Str {} then "string"
                case WeakComment {} | Comment {} then "comment"
                case _ then "default"
                end
            end) in
            concatAll ["<span class=\"", class, "\">", lit word, "</span>"]
        end) buffer) in
        
    
    recursive let work = lam tree: TreeSourceCode.
        switch tree
        case TreeSourceCodeNode arr then
            match sourceCodeSplit arr with { left = codeLeft, right = codeRight, trimmed = codeTrimmed } in
            let codeLeft = concatAll (map work codeLeft) in
            let codeRight = concatAll (map work codeRight) in
            let codeTrimmed = concatAll (map work codeTrimmed) in

            match codeRight with [] then concat codeLeft codeTrimmed
            else concatAll [codeLeft, (getHidenCode codeRight), codeTrimmed]
        case TreeSourceCodeSnippet buffer then getColorizedSnippet buffer
        end
    in
    
    let tree = getTreeSourceCode (ObjectNode { sons = sons, obj = obj }) in
    switch tree
    case TreeSourceCodeNode arr then
        let code = concatAll (map work arr) in
        concatAll ["<div class=\"inline-container\"><pre>",
        getHidenCode (cons '\n' code),
        "</pre></div>"]
                
    case TreeSourceCodeSnippet arr then
        warn "The top level node was a Snippet, should never happend here.";
        getColorizedSnippet arr
    end

-- Object pretty-printer with syntax coloring 
let objToStringColorized : Object -> String = use ObjectKinds in lam obj.
    let span = lam content. lam kind. concatAll ["<span class=\"", kind, "\">", content, "</span>"] in
    let kw = lam content. span content "kw" in
    let var = lam content. span content "var" in
    let tp = lam content. span content "tp" in

    switch obj.kind
    case ObjLet { rec = rec, args = args } then concatAll [if rec then concat (kw "recursive") " " else "", kw "let ", var obj.name, " ", strJoin " " (map var args)]
    case ObjType { t = t } then concatAll [kw "type", " ", var obj.name, match t with Some t then concat " : " (tp t) else ""]
    case ObjCon { t = t } then concatAll [kw "con", " ", var obj.name, " : ", tp t]
    case (ObjMexpr {} | ObjUtest {}) & kind then kw (getFirstWord kind)
    case ObjLang {} then concatAll [kw "lang", " ", tp obj.name]
    case ObjProgram {} then ""
    case kind then concatAll [kw (getFirstWord kind), " ", var obj.name]
    end

-- The HTML renderer implementation 
lang HtmlRenderer = RendererInterface + ObjectKinds

    syn Format =
        | Html {}

    sem formatFromStr =
        | "html" | "HTML" | "Html" | ".html" -> Some (Html {})

    sem objFormatHeader /- (Format, Object) -> String -/ =
        | (Html {}, obj) -> getHeader (objTitle obj)

    sem objFormatedTitle /- (Format, Object) -> String -/ =
    | (Html {}, obj) -> concat (htmlBalise (objTitle obj) "h1") "\n"

    sem getFormatedSectionTitle /- (Format, String) -> String -/ =
    | (Html {}, title) -> concat (htmlBalise (concat title ":") "h2") "\n"

    
    sem objFormat =
     | (Html {}, obj, sons) ->
        let s = objToStringColorized obj in
        
        match s with "" then "" else
        let link = objLink obj in
        concatAll [
        "<div style=\"position: relative;\">\n",
        htmlPre s, "\n",
        "<a href=\"", if strStartsWith "/" link then "" else "/", link, "\" style=\"
            position: absolute;
            bottom: 0.4em;
            right: 0.8em;
            font-size: 0.9em;
            color: #2980b9;
            text-decoration: none;
            \">[→]</a>
            </div>",
            htmlPre obj.doc,
            htmlBuildCodeSource obj sons
        ]

    sem objGetSpecificDoc =
    | (Html {}, { doc = doc, kind = ObjLang { parents = parents & ([_] ++ _) } } & obj, sons ) ->
        let parents = map htmlGetLangLink parents in
        concatAll [
        htmlStrong "Stem from:", "\n",
        (strJoin " + " parents), "\n<br>\n",
        htmlStrong "Signature:", "\n",
        objFormat (Html {}, obj, sons), "\n<br>\n"]

    | (Html {}, { doc = doc, kind = ( ObjSyn { langName = langName, variants = variants } | ObjSem { langName = langName, variants = variants } )} & obj, sons) ->
        let variants = concatAll (map (lam v. concatAll ["| ", v, "\n"]) variants) in
        concatAll [
            htmlStrong "From:", "\n", htmlGetLangLink langName, "\n\n",
            htmlBalise "Signature" "h2", "\n\n",
            htmlCode (concatAll [objToStringColorized obj, "\n", variants]), "\n",
            htmlDoc doc, "\n",
            htmlBuildCodeSource obj sons
         ]
    
    | (Html {}, obj, sons) ->
        let s = objToStringColorized obj in
        concatAll [
        match s with "" then "" else 
            concatAll [htmlBalise "Signature" "h2", "\n\n", (htmlCode s), "\n"],
            htmlDoc obj.doc, "\n",
            htmlBuildCodeSource obj sons]


    sem getFormatedLinkList /- (Format, [Object]) -> String -/ =
    | (Html {}, objects) ->
        let doc = map (lam u. htmlGetLink (objLink u) (objTitle u)) objects in
        let doc = strJoin ", " (reverse doc) in
        match doc with "" then "" else
            concat (htmlText doc) "\n"

    sem objFormatFooter /- (Format, Object) -> String -/ =
        | (Html {}, _) -> "</body>\n</html>"
end


    
