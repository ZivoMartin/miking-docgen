-- # Markdown Renderer for mi-doc-gen
--
-- This module implements the **MarkdownRenderer**, an instance of `RendererInterface`.
-- It generates Markdown pages from the extracted ObjectTree.

include "renderer-interface.mc"
include "../extracting/objects.mc"
include "../util.mc"
include "../logger.mc"
    
lang MarkdownRenderer = RendererInterface + ObjectKinds
    
    sem objFormatHeader /- (Format, Object) -> String -/ =
        | (Md {}, _) -> ""
    
    -- Markdown formatted display for an object    
    sem objFormat =
        | (Md {}, { obj = obj }) ->
            let s = objToString obj.kind obj.name in
            match s with "" then "" else concatAll ["```\n", s, "\n```\n\n[-](/", objLink obj, ")\n\n"]

    -- Markdown specific doc:
    -- - Lang shows parents
    -- - Sem / Syn shows language + variants
    -- - Let shows args
    sem objGetSpecificDoc =
    | ( Md {}, { obj = { doc = doc, kind = ObjLang { parents = parents & ([_] ++ _) } } } & data ) ->
        let parents = map (lam p. concatAll ["[", p, "](/", getLangLink p, ".lang)"]) parents in
        concatAll ["**Stem from:**  \n", (strJoin " + " parents), objFormat (Md {}, data), "\n\n", doc, "\n\n"]

    | (Md {}, { obj = { name = name, doc = doc, kind = ( ObjSyn { langName = langName, variants = variants } | ObjSem { langName = langName, variants = variants } ) & kind  } } ) ->
        let variants = concatAll (map (lam v. concatAll ["| ", v, "\n"]) variants) in
        concatAll [
            "From ", "[", langName, "](/", getLangLink langName, ".lang)  \n",
            "```\n", getFirstWord kind, " ", name, "\n", variants, "```\n\n", doc, "\n\n"
         ]
    
    | ( Md {}, { obj = obj } ) ->
        let s = objToString obj.kind obj.name in
        match s with "" then "" else concatAll ["```\n", s, "\n```\n\n", obj.doc, "\n\n"]


    sem objFormatedTitle /- (Format, Object) -> String -/ =
    | (Md {}, obj) -> concatAll ["# ", objTitle obj, "\n\n"]

    sem objGetFormatedLink /- (Format, Object) -> String -/ =
    | (Md {}, obj) -> concatAll ["[-](/", objLink obj,")\n\n"]

    sem getFormatedSectionTitle /- (Format, String) -> String -/ =
    | (Md {}, title) -> concatAll ["**", title, ":** \n\n"]

    sem getFormatedLinkList /- (Format, [Object]) -> String -/ =
    | (Md {}, objects) ->
        let doc = map (lam u. concatAll ["[", objTitle u, "](/", objLink u, ")"]) objects in
        concat (strJoin ", " (reverse doc)) "  \n\n"

    sem objFormatFooter /- (Format, Object) -> String -/ =
        | (Md {}, _) -> ""

    sem getWordRenderer =
        | Md {} -> lam. ""

    sem getCodeHider =
        | Md {} -> lam. ""

end
