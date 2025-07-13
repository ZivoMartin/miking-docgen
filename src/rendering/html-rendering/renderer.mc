-- # HTML Renderer for mi-doc-gen
--
-- This module implements the **HtmlRenderer**, an instance of `RendererInterface`.
-- It generates HTML pages from the extracted ObjectTree.

include "../../extracting/source-code-word.mc"
include "../renderer-interface.mc"
include "../type-colorizer.mc"
include "../../extracting/objects.mc"    
include "../source-code-spliter.mc"
include "./header.mc"
include "../source-code-reconstruction.mc"
include "../../logger.mc"
include "./utils.mc"

lang TypeColorizer = TypeColorizerTemplate

    sem formatTypeName =
    | t -> tp t

    sem formatEntryName =
    | t -> var t

    sem unknownDisplay =
    | () -> "Unknown"
end
    
-- Object pretty-printer with syntax coloring 
let objToStringColorized : Object -> String = use ObjectKinds in use TypeColorizer in use MExprPrettyPrint in lam obj.
    switch obj.kind
    case ObjLet { rec = rec, args = args, ty = ty } then
        let t = match ty with Some t then typeColorize t else "?" in
        let args = strJoin " " (map var args) in
        join [if rec then concat (kw "recursive") " " else "", kw "let ", var obj.name, " ", args, " : ", t]
    case ObjType { t = t } then join [kw "type", " ", var obj.name, match t with Some t then concat " : " (tp t) else ""]
    case ObjCon { t = t } then join [kw "con", " ", var obj.name, " : ", tp t]
    case (ObjMexpr {} | ObjUtest {}) & kind then kw (getFirstWord kind)
    case ObjLang {} then join [kw "lang", " ", tp obj.name]
    case ObjProgram {} then ""
    case ObjSem { ty = ty } then
        let t =
            match ty with Some t then typeColorize t else "?" in
        join [kw "sem", " ", var obj.name, " : ", t]
    case kind then join [kw (getFirstWord kind), " ", var obj.name]
    end

recursive let wordRenderer: WordRenderer = use TokenReader in use SourceCodeWordKinds in lam w.

    recursive let characterReplacer = lam s.
        switch s
        case "&" ++ s then concat "&amp;" (characterReplacer s)
        case "<" ++ s then concat "&lt;" (characterReplacer s)
        case ">" ++ s then concat "&gt;" (characterReplacer s)    
        case [x] ++ s then cons x (characterReplacer s)
        case "" then ""
        end
    in

    let renderSkiped: [Token] -> String = lam skiped.
        join (map (lam s. wordRenderer ( { word = s, kind = CodeDefault {} } )) skiped) in

    switch w
    case { word = Include { content = content, skiped = skiped } } then
        join [kw "include", renderSkiped skiped, st (characterReplacer content)]    
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
        let word = characterReplacer (lit word) in
        match class with "" then word else span word class
    end
end

let codeHider : Bool -> CodeHider = lam jumpLine. lam code.
    let jsDisplay = "<button class=\"toggle-btn\" onclick=\"toggle(this)\">...</button><div style=\"display: none;\">" in
    join [jsDisplay, if jumpLine then "\n" else "", code, "</div>"] 

let getCodeWithoutPreview = lam code.
    join ["<div class=\"inline-container\"><pre class=\"source\">", getCodeWithoutPreview (codeHider true) code, "</pre></div>"]

    
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
        join [
        "<div class=\"ObjectParent\">\n",
        htmlPre s, 
        "<a class=\"gotoLink\" href=\"", if strStartsWith "/" link then "" else "/", link, "\">[→]</a>", "\n",
        htmlPre obj.doc, code, "</div>"]

    sem objGetSpecificDoc =
    | (Html {}, { obj = { doc = doc, kind = ObjLang { parents = parents & ([_] ++ _) } } } & data ) ->
        let parents = map htmlGetLangLink parents in
        join [
        htmlStrong "Stem from:", "\n",
        (strJoin " + " parents), "\n<br>\n",
        htmlStrong "Signature:", "\n",
        objFormat (Html {}, data), "\n<br>\n"]

    | (Html {}, { obj = { doc = doc, kind = (
                ObjSyn { langName = langName, variants = variants } |
                ObjSem { langName = langName, variants = variants }
                )} & obj } & data ) ->
        let code = getCodeWithoutPreview data in
        let variants = join (map (lam v. join ["| ", v, "\n"]) variants) in
        join [
            htmlStrong "From:", "\n", htmlGetLangLink langName, "\n\n",
            htmlBalise "Signature" "h2", "\n\n",
            htmlCode (join [objToStringColorized obj, "\n", variants]), "\n",
            htmlDoc doc, "\n", code
         ]
    
    | (Html {}, { obj = obj } & data) ->
        let code = getCodeWithoutPreview data in
        let s = objToStringColorized obj in
        join [
        match s with "" then "" else 
            join [htmlBalise "Signature" "h2", "\n\n", (htmlCode s), "\n"],
            htmlDoc obj.doc, "\n", code]


    sem getFormatedLinkList /- (Format, [Object]) -> String -/ =
    | (Html {}, objects) ->
        let doc = map (lam u. htmlGetLink (objLink u) (objTitle u)) objects in
        let doc = strJoin ", " doc in
        match doc with "" then "" else
            concat (htmlText doc) "\n"

    sem objFormatFooter /- (Format, Object) -> String -/ =
        | (Html {}, _) -> "</div></body>\n</html>"

    sem getWordRenderer =
        | Html {} -> wordRenderer

    sem getCodeHider =
        | Html {} -> codeHider false
end
