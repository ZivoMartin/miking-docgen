-- # Renderer Interface
--
-- This module defines the interface for a renderer used in **mi-doc-gen**.
-- Note that each method gives a default result, this result corresponds to the Row format implementation 
--
-- Any renderer (MarkdownRenderer, HTMLRenderer, etc.) must implement this interface.

include "../extracting/objects.mc"
include "./rendering-types.mc"
include "../format.mc"
include "./source-code-spliter.mc"

lang RendererInterface = Formats + ObjectKinds + Colorizer

    -- Object pretty-printer with syntax coloring 
    sem renderStringColorized : Object -> Format -> String
    sem renderStringColorized (obj : Object) =
    | fmt | Row { fmt = fmt } ->
    let name = objName obj in
    let kind = objKind obj in
    let code = switch obj.kind
    case ObjLet { rec = rec, args = args, ty = ty } then
        let t = match ty with Some t then typeColorize t else "?" in
        let args = strJoin " " (map var args) in
        join [if rec then "recursive " else "", "let ", name, " ", args, " : ", t]
    case ObjType { t = t } then
        join ["type ", name, match t with Some t then concat " : " t else ""]
    case ObjCon { t = t } then
        join ["con ", name, " : ", t]
    case (ObjMexpr {} | ObjUtest {}) & kind then
        getFirstWord kind
    case ObjLang {} then
        concat "lang " name
    case ObjProgram {} then ""
    case ObjSem { ty = ty } then
        let t = match ty with Some t then typeColorize t else "?" in
        join ["sem ", name, " : ", t]
    case kind then
        join [getFirstWord kind, " ", name]
    end in
    renderSourceCode (strToSourceCode code) fmt

    
    -- Render a list of objects as a link list, separated by commas
    sem renderLinkList : [Object] -> Format -> String
    sem renderLinkList (objects: [Object]) =
    | fmt | Row { fmt = fmt } ->
        let doc = map (lam u. renderLink (objLink u) (objTitle u) fmt) objects in
        let doc = strJoin ", " doc in
        match doc with "" then "" else
            concat (renderText doc fmt) (renderNewLine fmt)

    sem renderGotoLink : String -> Format -> String
    sem renderGotoLink (link: String) =
    | _ -> link

    sem renderDoc : String -> Format -> String
    sem renderDoc (doc: String) =
    | _ -> doc

    sem renderLabel : String -> Format -> String
    sem renderLabel (label: String) =
    | _ -> label
    
    sem renderRenderingData : RenderingData -> Format -> String
    sem renderRenderingData (data : RenderingData) =
    | fmt | Row { fmt = fmt } ->
        match data with { obj = obj } in
        let code = renderCodeWithoutPreview data fmt in
        let s = renderStringColorized obj fmt in
        match s with "" then "" else
        let link = objLink obj in
        join [renderLabel s, 
        renderGotoLink (concat (if strStartsWith "/" link then "" else "/") link) fmt, renderNewLine fmt,
        renderDoc (objDoc obj), code]

    sem renderSpecificDoc : RenderingData -> Format -> String
    sem renderSpecificDoc (data: RenderingData) =
    | fmt | Row { fmt = fmt } ->
        let nl = renderNewLine fmt in
        switch data
        case { obj = { doc = doc, kind = ObjLang { parents = parents & ([_] ++ _) } } } then
            let parents = map (lam p. renderLink fmt p) parents in
            strJoin (nl) [
            renderBold "Stem from:" fmt,
            (strJoin " + " parents),
            renderBold "Signature:" fmt,
            renderRenderingData data fmt, ""] -- Last element is to force newline with strJoin
        case { obj = { doc = doc, kind = (
                ObjSyn { langName = langName, variants = variants } |
                ObjSem { langName = langName, variants = variants }
                )} & obj } then
            let variants = join (map (lam v.
                join ["| ", renderWord (buildCodeWord (Word { content = v }) (CodeName {})), nl]
                ) variants) in
            strJoin (nl) [
            renderBold "From:" fmt,
            renderLink langName (concat (getLangLink langName) ".lang") fmt,
            renderTitle 2 "Signature:" fmt,
            renderLabel (join [renderStringColorized obj, "\n", variants]) fmt,
            renderDoc doc fmt,
            renderCodeWithoutPreview data fmt, ""]
        case { obj = obj } then
            let code = renderCodeWithoutPreview data fmt in
            let s = renderStringColorized obj fmt in
            join [
            match s with "" then "" else join [renderTitle 2 "Signature" fmt, nl, renderLabel s, nl],
            renderDoc (objDoc obj), nl, code]
        end


    -- Concatenates the left and right parts of the code without showing a preview.
    -- The hider is applied to the full right side.
    sem renderCodeWithoutPreview : RenderingData -> Format -> String
    sem renderCodeWithoutPreview (data: RenderingData) = 
    | fmt | Row { fmt = fmt } -> renderHidenCode (concat data.left data.right) fmt

    -- Applies preview logic to the code rendering.
    -- - If `right` is empty, then all content is shown directly.
    -- - Otherwise, only `left` is visible and `right` is hidden via the hider.
    -- - `trimmed` is always shown, as a minimal representation (e.g., `...` or type info)    
    sem renderCodeWithPreview : RenderingData -> Format -> String
    sem renderCodeWithPreview (data: RenderingData) =
    | fmt | Row { fmt = fmt } ->
        match data.right with [] then
            join [data.left, data.trimmed]
        else 
            join [data.left, renderHidenCode data.right fmt, data.trimmed]

    sem renderHidenCode : String -> Format -> String
    sem renderHidenCode (code : String) =
    | _ -> ""

    -- Render the header of the page for an object
    sem renderHeader : Object -> Format -> String
    sem renderHeader (obj : Object) =
    | _ -> ""

    -- Render the footer of the page for an object
    sem renderFooter : Object -> Format -> String
    sem renderFooter (obj : Object) =
    | _ -> ""

    sem renderSectionTitle : String -> Format -> String
    sem renderSectionTitle (title: String) =
    | fmt -> renderTitle (renderBold (concat title) fmt) fmt

    sem renderBold : String -> Format -> String
    sem renderBold (text : String) =
    | _ -> text

    sem renderRemoveForbidenChars : String -> String
    sem renderRemoveForbidenChars (s: String) =
    | _ -> s

    sem renderSourceCode : SourceCode -> Format -> String
    sem renderSourceCode (code: SourceCode) =
    | fmt | Row { fmt = fmt } ->
        join (map (lam code. match code with Some code then renderWord code fmt else "") code)
    
    sem renderWord : SourceCodeWord -> Format -> String
    sem renderWord (word: SourceCodeWord) = 
    | fmt | Row { fmt = fmt } -> 
        let renderSkiped: [Token] -> String = lam skiped.
            join (map (lam s. renderWord ( { word = s, kind = CodeDefault {} } )) skiped) in

        switch word
        case { word = Include { content = content, skiped = skiped } } then
            join [renderKeyword "include" fmt, renderSkiped skiped, renderString (renderRemoveForbidenChars content fmt) fmt]    
        case { word = word, kind = kind } then
            let renderer = (switch word
            case Str {} then renderString
            case WeakComment {} then renderWeakComment
            case Comment {} then renderComment
            case _ then
                switch kind
                case CodeKeyword {} then renderKeyword
                case CodeName {} then renderVar
                case CodeType {} then renderType
                case CodeDefault {} then renderDefault
                end       
            end) in
            let word = renderRemoveForbidenChars (lit word) fmt in
            renderer word fmt
        end

    sem renderTreeSourceCode : TreeSourceCode -> Object -> Format -> RenderingData
    sem renderTreeSourceCode (tree: [TreeSourceCode]) (obj : Object) =
    | fmt ->
        match sourceCodeSplit tree with { left = left, right = right, trimmed = trimmed } in

        let renderSourceCode = lam b. renderSourceCode (wordBufferToSourceCode b) fmt in
    
        let getFormatedString : [TreeSourceCode] -> String = lam code.
            foldl (lam s. lam node.
                concat (switch node 
                case TreeSourceCodeNode son then renderCodeWithPreview son fmt
                case TreeSourceCodeSnippet code then renderSourceCode code
                end) s
                ) "" (reverse code) in

        {
            obj = obj,
            left = getFormatedString left,
            right = getFormatedString right,
            trimmed = switch trimmed
                case TrimmedFormated s then s
                case TrimmedNotFormated b then renderSourceCode b
                end
        }


    sem renderTitle : Int -> Object -> Format -> String
    sem renderTitle (size : Int) (obj : Object) =
    | _ -> objTitle obj
    
    sem renderText : String -> Format -> String
    sem renderText (text : String) =
    | _ -> text

    sem renderLink : (Format, String, String) -> String
    sem renderLink (title : String) (link : String) =
    | _ -> join [title, " (", link, ")"]
    
    sem renderType : String -> Format -> String
    sem renderType (content : String) = 
    | _ -> content

    sem renderVar : String -> Format -> String
    sem renderVar (content : String) =
    | _ -> content
    
    sem renderKeyword : String -> Format -> String
    sem renderKeyword (content : String) =
    | _ -> content
    
    sem renderComment : String -> Format -> String
    sem renderComment (content : String) =
    | _ -> content
    
    sem renderString : String -> Format -> String
    sem renderString (content : String) =
    | _ -> content
    
    sem renderDefault : String -> Format -> String
    sem renderDefault (content : String) =
    | _ -> content
    
    sem renderWeakComment : String -> Format -> String
    sem renderWeakComment (content : String) =
    | _ -> content

    sem renderNewLine : Format -> String
    sem renderNewLine (content : String) =
    | _ -> "\n"
    
end
