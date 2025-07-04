-- # HTML Renderer for mi-doc-gen
--
-- This module implements the **HtmlRenderer**, an instance of `RendererInterface`.
-- It generates HTML pages from the extracted ObjectTree.

include "../extracting/source-code-word.mc"
include "renderer-interface.mc"
include "../extracting/objects.mc"    
include "./source-code-spliter.mc"
include "./html-header.mc"
include "./source-code-reconstruction.mc"
include "../logger.mc"

-- HTML helpers
let htmlBalise = lam s. lam b. concatAll ["<", b, ">\n", s, "\n</", b, ">"]
let htmlText = lam s. htmlBalise s "p"
let htmlPre = lam s. concatAll ["<pre>", s, "</pre>"]
let htmlCode = lam s. concatAll ["<pre class=code>", s, "</pre>"]
let htmlStrong = lam s. htmlText (htmlBalise s "strong")

let htmlGetLink = lam l. lam txt. concatAll ["<a href=\"/", l, "\">", txt, "</a>"]
let htmlGetLangLink = lam lng. htmlGetLink (concat (getLangLink lng) ".lang") lng

let htmlDoc = lam doc. concatAll ["<pre class=md>", doc, "</pre>"]                

let span = lam content. lam kind. concatAll ["<span class=\"", kind, "\">", content, "</span>"]
let kw = lam content. span content "kw"
let var = lam content. span content "var"
let tp = lam content. span content "tp"
let st = lam content. span content "string"

-- Object pretty-printer with syntax coloring 
let objToStringColorized : Object -> String = use ObjectKinds in lam obj.
    switch obj.kind
    case ObjLet { rec = rec, args = args } then concatAll [if rec then concat (kw "recursive") " " else "", kw "let ", var obj.name, " ", strJoin " " (map var args)]
    case ObjType { t = t } then concatAll [kw "type", " ", var obj.name, match t with Some t then concat " : " (tp t) else ""]
    case ObjCon { t = t } then concatAll [kw "con", " ", var obj.name, " : ", tp t]
    case (ObjMexpr {} | ObjUtest {}) & kind then kw (getFirstWord kind)
    case ObjLang {} then concatAll [kw "lang", " ", tp obj.name]
    case ObjProgram {} then ""
    case kind then concatAll [kw (getFirstWord kind), " ", var obj.name]
    end

recursive let wordRenderer: WordRenderer = use TokenReader in use SourceCodeWordKinds in lam w.
    let renderSkiped: [Token] -> String = lam skiped.
        concatAll (map (lam s. wordRenderer ( { word = s, kind = CodeDefault {} } )) skiped) in
    switch w
    case { word = Include { content = content, skiped = skiped } } then
        concatAll [kw "include", renderSkiped skiped, st content]    
    case { word = Recursive { skiped = skiped } } then
        concatAll [kw "recursive", renderSkiped skiped, kw "let"]
    case { word = word, kind = kind } then
        let class = (switch word
        case Str {} then "string"
        case WeakComment {} | Comment {} then "comment"
        case _ then
            switch kind
            case CodeKeyword {} then "kw"
            case CodeName {} then "var"
            case CodeType {} then "tp"
            case CodeDefault {} then ""
            end       
        end) in
        let word = lit word in
        match class with "" then word else span word class
    end
end

let codeHider : Bool -> CodeHider = lam jumpLine. lam code.
    let jsDisplay = "<button class=\"toggle-btn\" onclick=\"toggle(this)\">...</button><div style=\"display: none;\">" in
    concatAll [jsDisplay, if jumpLine then "\n" else "", code, "</div>"] 

let getCodeWithoutPreview = lam code.
    concatAll ["<div class=\"inline-container\"><pre class=\"source\">", getCodeWithoutPreview (codeHider true) code, "</pre></div>"]

    
    
-- The HTML renderer implementation 
lang HtmlRenderer = RendererInterface + ObjectKinds

    sem objFormatHeader /- (Format, Object) -> String -/ =
        | (Html {}, obj) -> getHeader (objTitle obj)

    sem objFormatedTitle /- (Format, Object) -> String -/ =
    | (Html {}, obj) -> concat (htmlBalise (objTitle obj) "h1") "\n"

    sem getFormatedSectionTitle /- (Format, String) -> String -/ =
    | (Html {}, title) -> concat (htmlBalise (concat title ":") "h2") "\n"

    
    sem objFormat =
     | (Html {}, { obj = obj } & data) ->
        let code = getCodeWithoutPreview data in
        let s = objToStringColorized obj in
        match s with "" then "" else
        let link = objLink obj in
        concatAll [
        "<div class=\"ObjectParent\">\n",
        htmlPre s, "\n",
        "<a class=\"gotoLink\" href=\"", if strStartsWith "/" link then "" else "/", link, "\">[→]</a>",
        htmlPre obj.doc, code, "</div>"]

    sem objGetSpecificDoc =
    | (Html {}, { obj = { doc = doc, kind = ObjLang { parents = parents & ([_] ++ _) } } } & data ) ->
        let parents = map htmlGetLangLink parents in
        concatAll [
        htmlStrong "Stem from:", "\n",
        (strJoin " + " parents), "\n<br>\n",
        htmlStrong "Signature:", "\n",
        objFormat (Html {}, data), "\n<br>\n"]

    | (Html {}, { obj = { doc = doc, kind = (
                ObjSyn { langName = langName, variants = variants } |
                ObjSem { langName = langName, variants = variants }
                )} & obj } & data ) ->
        let code = getCodeWithoutPreview data in
        let variants = concatAll (map (lam v. concatAll ["| ", v, "\n"]) variants) in
        concatAll [
            htmlStrong "From:", "\n", htmlGetLangLink langName, "\n\n",
            htmlBalise "Signature" "h2", "\n\n",
            htmlCode (concatAll [objToStringColorized obj, "\n", variants]), "\n",
            htmlDoc doc, "\n", code
         ]
    
    | (Html {}, { obj = obj } & data) ->
        let code = getCodeWithoutPreview data in
        let s = objToStringColorized obj in
        concatAll [
        match s with "" then "" else 
            concatAll [htmlBalise "Signature" "h2", "\n\n", (htmlCode s), "\n"],
            htmlDoc obj.doc, "\n", code]


    sem getFormatedLinkList /- (Format, [Object]) -> String -/ =
    | (Html {}, objects) ->
        let doc = map (lam u. htmlGetLink (objLink u) (objTitle u)) objects in
        let doc = strJoin ", " (reverse doc) in
        match doc with "" then "" else
            concat (htmlText doc) "\n"

    sem objFormatFooter /- (Format, Object) -> String -/ =
        | (Html {}, _) -> "</div></body>\n</html>"

    sem getWordRenderer =
        | Html {} -> wordRenderer

    sem getCodeHider =
        | Html {} -> codeHider false
end
