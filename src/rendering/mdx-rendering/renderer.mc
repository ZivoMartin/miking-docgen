-- # MDX Renderer for mi-doc-gen
--
-- This module implements the **MdxRenderer**, an instance of `RendererInterface`.
-- It generates MDX pages from the extracted ObjectTree.

include "../../extracting/source-code-word.mc"
include "../renderer-interface.mc"
include "../type-colorizer.mc"
include "../../extracting/objects.mc"    
include "../source-code-spliter.mc"
include "../source-code-reconstruction.mc"
include "../../logger.mc"
include "../rendering-types.mc"

-- lang TypeColorizer = TypeColorizerTemplate

--     sem formatTypeName =
--     | t -> tp t

--     sem formatEntryName =
--     | t -> var t

--     sem unknownDisplay =
--     | () -> "Unknown"
-- end
    
-- Object pretty-printer with syntax coloring 
let objToStringColorized : Object -> String = lam obj.
    renderingWarn "TODO: objToStringColorized for MDX"; "TODO: objToStringColorized"

-- recursive let mdxWordRenderer: WordRenderer = use TokenReader in use SourceCodeWordKinds in lam w.

--     recursive let characterReplacer = lam s.
--         switch s
--         case "&" ++ s then concat "&amp;" (characterReplacer s)
--         case "<" ++ s then concat "&lt;" (characterReplacer s)
--         case ">" ++ s then concat "&gt;" (characterReplacer s)    
--         case [x] ++ s then cons x (characterReplacer s)
--         case "" then ""
--         end
--     in

--     let renderSkiped: [Token] -> String = lam skiped.
--         join (map (lam s. mdxWordRenderer ( { word = s, kind = CodeDefault {} } )) skiped) in

--     switch w
--     case { word = Include { content = content, skiped = skiped } } then
--         join [kw "include", renderSkiped skiped, st (characterReplacer content)]    
--     case { word = word, kind = kind } then
--         let class = (switch word
--         case Str {} then "string"
--         case WeakComment {} | Comment {} then "comment"
--         case _ then
--             switch kind
--             case CodeKeyword {} then "kw"
--             case CodeName {} then "var"
--             case CodeType {} then "tp"
--             case CodeDefault {} then ""
--             end       
--         end) in
--         let word = characterReplacer (lit word) in
--         match class with "" then word else span word class
--     end
-- end

let mdxCodeHider = lam s. renderingWarn "todo: mdxCodeHider for MDX"; "todo: mdxCodeHider for MDX"
let mdxWordRenderer = lam s. renderingWarn "todo: WordRenderer for MDX"; "todo: WordRenderer for MDX"
let mdGetLink = lam link. lam title. join ["[", title, "](", link, ")"]
let mdStrong = lam text. join ["**", text, "**"]
let mdTitle = lam size. lam title. join [join (repeat "#" size), " ", title, "\n"]
    
-- The Mdx renderer implementation 
lang MdxRenderer = RendererInterface + ObjectKinds

    sem objFormatHeader =
    | (Mdx {}, obj) -> ""

    sem objFormatedTitle =
    | (Mdx {}, obj) -> mdTitle 1 (objTitle obj)

    sem getFormatedSectionTitle =
    | (Mdx {}, title) -> mdTitle 2 (concat title ":")

    sem newLine =
    | Mdx {} -> "  \n"
    
    sem objFormat =
     | (Mdx {}, { obj = obj } & data) ->
        let code = getCodeWithoutPreview mdxCodeHider data in
        let s = objToStringColorized obj in
        match s with "" then "" else
        let link = objLink obj in
        let link = concat (if strStartsWith "/" link then "" else "/") link in
        join [s, mdGetLink link "[→]", newLine (Mdx {}), obj.doc, code, newLine (Mdx {})]

    sem objGetSpecificDoc =
    | (Mdx {}, { obj = { doc = doc, kind = ObjLang { parents = parents & ([_] ++ _) } } } & data ) ->
        let parents = map htmlGetLangLink parents in
        join [
        mdStrong "Stem from:", newLine (Mdx {}),
        (strJoin " + " parents), newLine (Mdx {}),
        mdStrong "Signature:", newLine (Mdx {}),
        objFormat (Mdx {}, data), newLine (Mdx {}), newLine (Mdx {})]

    | (Mdx {}, { obj = { doc = doc, kind = (
                ObjSyn { langName = langName, variants = variants } |
                ObjSem { langName = langName, variants = variants }
                )} & obj } & data ) ->
        let code = getCodeWithoutPreview mdxCodeHider data in
        let variants = join (map (lam v. join ["| ", v, newLine (Mdx {})]) variants) in
        join [
            mdStrong "From:", newLine (Mdx {}), htmlGetLangLink langName, newLine (Mdx {}), newLine (Mdx {}),
            mdTitle 2 "Signature", newLine (Mdx {}),
            objToStringColorized obj, newLine (Mdx {}), variants, newLine (Mdx {}),
            doc, newLine (Mdx {}), code, newLine (Mdx {})
         ]
    
    | (Mdx {}, { obj = obj } & data) ->
        let code = getCodeWithoutPreview mdxCodeHider data in
        let s = objToStringColorized obj in
        join [
        match s with "" then "" else 
            join [mdTitle 2 "Signature", newLine (Mdx {}), s, newLine (Mdx {})],
            obj.doc, newLine (Mdx {}), code]


    sem getFormatedLinkList =
    | (Mdx {}, objects) ->
        let doc = map (lam u. mdGetLink (objLink u) (objTitle u)) objects in
        let doc = strJoin ", " doc in
        match doc with "" then "" else
            concat doc (newLine (Mdx {}))

    sem objFormatFooter =
        | (Mdx {}, _) -> ""

    sem getWordRenderer =
        | Mdx {} -> mdxWordRenderer

    sem getCodeHider =
        | Mdx {} -> mdxCodeHider
end

