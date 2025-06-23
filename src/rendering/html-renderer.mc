include "renderer-interface.mc"

lang HtmlRenderer = RendererInterface

    syn Format =
        | Html {}

    sem formatFromStr =
        | "html" | "HTML" | "Html" | ".html" -> Some (Html {})

    sem objFormat /- (Format, Object) -> String -/ =
        | (Html {}, obj) ->
            let s = objToString obj.kind obj.name in
            match s with "" then "" else concatAll ["\n\n```\n", s, "\n```\n\n"]

    -- Markdown specific doc:
    -- - Lang shows parents
    -- - Sem / Syn shows language + variants
    -- - Let shows args
    sem objGetSpecificDoc /- (Format, Object) -> String -/ =
    | ( Md {}, { kind = ObjLang { parents = parents & ([_] ++ _) } } & obj ) ->
        let parents = map (lam p. concatAll ["[", p, "](/", getLangLink p, ".lang)"]) parents in
        concatAll ["**Stem from:**\n\n ", (strJoin " + " parents), objFormat (Md {}, obj)]

    | (Md {}, { name = name, kind = ( ObjSyn { langName = langName, variants = variants } | ObjSem { langName = langName, variants = variants } ) & kind } ) ->
        let variants = concatAll (map (lam v. concatAll ["| ", v, "\n"]) variants) in
        concatAll [
            "From ", "[", langName, "](/", getLangLink langName, ".lang)\n\n",
            "```\n", getFirstWord kind, " ", name, "\n", variants, "```\n\n"
         ]
    
    | ( Md {}, obj ) ->
        let s = objToString obj.kind obj.name in
        match s with "" then "" else concatAll ["\n\n```\n", s, "\n```\n\n"]


    sem objFormatedTitle /- (Format, Object) -> String -/ =
    | (Md {}, obj) -> concatAll ["# ", objTitle obj, "\n\n"]

    sem objGetFormatedLink /- (Format, Object) -> String -/ =
    | (Md {}, obj) -> concatAll ["\n[-](/", objLink obj,")\n\n"]

    sem getFormatedSectionTitle /- (Format, String) -> String -/ =
    | (Md {}, title) -> concatAll ["**", title, ":** \n\n"]

    sem getFormatedLinkList /- (Format, [Object]) -> String -/ =
    | (Md {}, objects) ->
        let doc = map (lam u. concatAll ["[", objTitle u, "](/", objLink u, ")"]) objects in
        concat (strJoin ", " (reverse doc)) "\n\n"

end
