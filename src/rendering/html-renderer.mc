include "renderer-interface.mc"
include "../extracting/objects.mc"    


    
let htmlBalise = lam s. lam b. concatAll ["<", b, ">", s, "</", b, ">"]
let htmlText = lam s. htmlBalise s "p"
let htmlCode = lam s. htmlText (htmlBalise s "code")
let htmlStrong = lam s. htmlText (htmlBalise s "strong")

let htmlGetLink = lam l. lam txt. concatAll ["<a href=\"/", l, "\">", txt, "</a>"]
let htmlGetLangLink = lam lng. htmlGetLink (concat (getLangLink lng) ".lang") lng

lang HtmlRenderer = RendererInterface + ObjectKinds

    syn Format =
        | Html {}

    sem formatFromStr =
        | "html" | "HTML" | "Html" | ".html" -> Some (Html {})

    sem objFormatHeader /- (Format, Object) -> String -/ =
        | (Html {}, obj) -> concatAll 
["<!DOCTYPE html>
<html>
    <head>
        <meta charset=\"utf-8\">
        <title>", objTitle obj, "</title>
        <style>
            body {{ font-family: sans-serif; max-width: 800px; margin: auto; padding: 2em; }}
            h1, h2, h3 {{ color: #444; }}
            pre, code {{ background: #f4f4f4; padding: 0.2em 0.4em; }}
        </style>
    </head>
    <body>
"]

    
    sem objFormatedTitle /- (Format, Object) -> String -/ =
    | (Html {}, obj) -> concat (htmlBalise (objTitle obj) "h1") "\n"

    sem getFormatedSectionTitle /- (Format, String) -> String -/ =
    | (Html {}, title) -> concat (htmlStrong (concat title ":") ) "\n"

    
    sem objFormat /- (Format, Object) -> String -/ =
     | (Html {}, obj) ->
        let s = objToString obj.kind obj.name in
        match s with "" then "" else concat (htmlCode s) "\n"

    sem objGetSpecificDoc /- (Format, Object) -> String -/ =
    | (Html {}, { kind = ObjLang { parents = parents & ([_] ++ _) } } & obj ) ->
        let parents = map htmlGetLangLink parents in
        concatAll [
        htmlStrong "Stem from:", "\n",
        (strJoin " + " parents), objFormat (Html {}, obj), "\n"]

    | (Html {}, { name = name, kind = ( ObjSyn { langName = langName, variants = variants } | ObjSem { langName = langName, variants = variants } ) & kind } ) ->
        let variants = concatAll (map (lam v. concatAll ["| ", v, "\n"]) variants) in
        concatAll [
            htmlStrong "From", " ", htmlGetLangLink langName, "\n",
            htmlCode (concatAll [getFirstWord kind, " ", name, "\n", variants]), "\n"
         ]
    
    | (Html {}, obj ) ->
        let s = objToString obj.kind obj.name in
        match s with "" then "" else concat (htmlCode s) "\n"


    sem objGetFormatedLink /- (Format, Object) -> String -/ =
    | (Html {}, obj) -> concat (htmlGetLink (objLink obj) "-") "\n"


    sem getFormatedLinkList /- (Format, [Object]) -> String -/ =
    | (Html {}, objects) ->
        let doc = map (lam u. htmlGetLink (objLink u) (objTitle u)) objects in
        concat (strJoin ", " (reverse doc)) "\n"

    sem objFormatFooter /- (Format, Object) -> String -/ =
        | (Html {}, _) -> "</body>\n</html>"
end


    
