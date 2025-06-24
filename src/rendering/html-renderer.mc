include "renderer-interface.mc"
include "../extracting/objects.mc"    


    
let htmlBalise = lam s. lam b. concatAll ["<", b, ">", s, "</", b, ">"]
let htmlText = lam s. htmlBalise s "p"
let htmlCode = lam s. concatAll ["<pre class=code>", s, "</pre>"]
let htmlStrong = lam s. htmlText (htmlBalise s "strong")

let htmlGetLink = lam l. lam txt. concatAll ["<a href=\"/", l, "\">", txt, "</a>"]
let htmlGetLangLink = lam lng. htmlGetLink (concat (getLangLink lng) ".lang") lng

let htmlDoc = lam doc. concatAll ["<pre class=md>", doc, "</pre>"]
    
let objToStringColorized = use ObjectKinds in lam obj.
    let span = lam content. lam kind. concatAll ["<span class=\"", kind, "\">", content, "</span>"] in
    let kw = lam content. span content "kw" in
    let var = lam content. span content "var" in
    let arg = lam content. span content "arg" in
    let tp = lam content. span content "tp" in

    switch obj.kind
    case ObjLet { rec = rec, args = args } then concatAll [if rec then concat (kw "recursive") " " else "", kw "let ", var obj.name, " ", strJoin " " (map arg args)]
    case ObjType { t = t } then concatAll [kw "type", " ", var obj.name, match t with Some t then concat " : " (tp t) else ""]
    case ObjCon { t = t } then concatAll [kw "con", " ", var obj.name, " : ", tp t]
    case ObjMexpr {} then kw "mexpr"
    case ObjProgram {} then ""
    case kind then concatAll [kw (getFirstWord kind), " ", var obj.name]
    end
    
lang HtmlRenderer = RendererInterface + ObjectKinds

    syn Format =
        | Html {}

    sem formatFromStr =
        | "html" | "HTML" | "Html" | ".html" -> Some (Html {})

    sem objFormatHeader /- (Format, Object) -> String -/ =
        | (Html {}, obj) -> concatAll [
"<!DOCTYPE html>
<html lang=\"en\">
<head>
<meta charset=\"utf-8\">
<title>", objTitle obj, "</title>
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
<style>
    body {
        font-family: system-ui, sans-serif;
        max-width: 900px;
        margin: auto;
        padding: 2em;
        line-height: 1.6;
        background-color: #fdfdfd;
        color: #333;
    }

    h1 {
        color: #2c3e50;
        border-bottom: 2px solid #eee;
        padding-bottom: 0.3em;
    }

    h2 {
        color: #2c3e50;
        font-size: 1.1em;
        margin-top: 3em;
        border-bottom: 1px solid #eee;
        padding-bottom: 0.3em;
    }

    a {
        color: #2980b9;
        text-decoration: none;
    }
    
    a:hover {
        text-decoration: underline;
    }

    pre.code {
        font-family: Consolas, \"Liberation Mono\", Menlo, Courier, monospace;
        background: #f5f5f5;
        border-left: 4px solid #2980b9;
        padding: 1em;
        overflow-x: auto;
        border-radius: 5px;
        font-size: 0.85em; 
        line-height: 1.4;
        margin-bottom: 2em;
    }

    pre a:hover {
        text-decoration: underline;
    }

    pre span {
        font-family: inherit;
        font-size: inherit;
    }

    
    pre.md {
        font-family: system-ui, sans-serif;
        font-size: 0.95em;
        line-height: 1.6;
        white-space: pre-wrap;
    }

    nav {
        background: #fafafa;
        padding: 0.8em 1em;
        border-bottom: 1px solid #ddd;
        margin-bottom: 2em;
        font-size: 0.9em;
        color: #999;
    }

    nav a {
        margin-right: 1em;
        color: #999;
        text-decoration: none;
        cursor: default; /* pas la main cliqueuse */
    }

    nav a:hover {
        text-decoration: none;
    }
    
    .kw  { color: #d73a49; }   
    .var { color: #005cc5; }                      
    .arg { color: #24292e; font-style: italic; }  
    .tp  { color: #22863a; }

    </style>
</head>
<body>
<nav>
    <a href=\"/\">Home (todo)</a>
    <a href=\"/Lang/\">Modules (todo)</a>
</nav>"]

    
    sem objFormatedTitle /- (Format, Object) -> String -/ =
    | (Html {}, obj) -> concat (htmlBalise (objTitle obj) "h1") "\n"

    sem getFormatedSectionTitle /- (Format, String) -> String -/ =
    | (Html {}, title) -> concat (htmlBalise (concat title ":") "h2") "\n"

    
    sem objFormat /- (Format, Object) -> String -/ =
     | (Html {}, obj) ->
        let s = objToStringColorized obj in
        
        match s with "" then "" else
        concatAll [
        "<div style=\"position: relative;\">\n",
        htmlBalise s "pre", "\n",
        "<a href=\"", objLink obj, "\" style=\"
            position: absolute;
            bottom: 0.4em;
            right: 0.8em;
            font-size: 0.9em;
            color: #2980b9;
            text-decoration: none;
            \">[→]</a>
        </div>\n"
        ]

    sem objGetSpecificDoc /- (Format, Object) -> String -/ =
    | (Html {}, { doc = doc, kind = ObjLang { parents = parents & ([_] ++ _) } } & obj ) ->
        let parents = map htmlGetLangLink parents in
        concatAll [
        htmlStrong "Stem from:", "\n",
        (strJoin " + " parents), objFormat (Html {}, obj), "\n", htmlDoc doc]

    | (Html {}, { doc = doc, kind = ( ObjSyn { langName = langName, variants = variants } | ObjSem { langName = langName, variants = variants } )} & obj ) ->
        let variants = concatAll (map (lam v. concatAll ["| ", v, "\n"]) variants) in
        concatAll [
            htmlStrong "From:", htmlGetLangLink langName, "\n\n",
            htmlBalise "Signature" "h2", "\n\n",
            htmlCode (concatAll [objToStringColorized obj, "\n", variants]), "\n",
            htmlDoc doc, "\n" 
         ]
    
    | (Html {}, obj ) ->
        let s = objToStringColorized obj in
        concatAll [
        match s with "" then "" else 
            concatAll [htmlBalise "Signature" "h2", "\n\n", (htmlCode s), "\n"],
            htmlDoc obj.doc, "\n"]


    sem objGetFormatedLink /- (Format, Object) -> String -/ =
    | (Html {}, obj) -> concat (htmlText (htmlGetLink (objLink obj) "-")) "\n"


    sem getFormatedLinkList /- (Format, [Object]) -> String -/ =
    | (Html {}, objects) ->
        let doc = map (lam u. htmlGetLink (objLink u) (objTitle u)) objects in
        let doc = strJoin ", " (reverse doc) in
        match doc with "" then "" else
            concat (htmlText doc) "\n"

    sem objFormatFooter /- (Format, Object) -> String -/ =
        | (Html {}, _) -> "</body>\n</html>"
end


    
