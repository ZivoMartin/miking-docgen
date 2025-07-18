include "../source-code-spliter.mc"
include "./renderer-interface.mc"

lang RowRenderer = RendererInterface

    sem renderBlocDefault : RenderingData -> Format -> String -> String -> String -> String -> String
    sem renderBlocDefault =
    | { obj = obj } & data -> lam fmt. lam bonusTopDoc. lam bonusSignDescDoc. lam bonusDescCodeDoc. lam bonusBottomDoc.
        let signature = renderDocSignature obj fmt in
        let description = renderDocDescription obj fmt in
        let code = renderCodeWithoutPreview data fmt in

        join [bonusTopDoc, signature, bonusSignDescDoc, description, bonusDescCodeDoc, code, bonusBottomDoc] 
            
    sem renderTopPageDoc (data: RenderingData) =
    | fmt -> let fmt = unwrapRow fmt in
        let nl = renderNewLine fmt in
        let top = switch data
        case { obj = { kind = ObjLang { parents = parents & ([_] ++ _) } } } then
            let parents = strJoin " + " (map (lam p. renderLink p (concat (getLangLink p) ".lang") fmt) parents) in
            let sectionTitle = renderSectionTitle "Stem from:" fmt in
            join [sectionTitle, nl, nl, parents]
        case { obj = { kind = ( ObjSyn {} | ObjSem {} )} & obj } then
            let langName = objGetLangName obj in
            let langName = renderLink langName (concat (getLangLink langName) ".lang") fmt in
            let sectionTitle = renderSectionTitle "From:" fmt in
            concat sectionTitle langName
        case { obj = obj } then
            ""
        end in
        renderBlocDefault data fmt top "" "" ""
    
    sem renderDocBloc (data : RenderingData) =
    | fmt -> let fmt = unwrapRow fmt in
        match data with { obj = obj } in
        let link = objLink obj in
        let link = concat (if strStartsWith "/" link then "" else "/") link in
        let link = renderGotoLink link fmt in
        renderBlocDefault data fmt "" "" link ""
    
    sem renderDocDescription (obj: Object) =
    | fmt -> let fmt = unwrapRow fmt in
        let doc = objDoc obj in
        concat (renderRemoveForbidenChars doc fmt) (renderNewLine fmt)

    sem renderDocSignature (obj : Object) =
    | fmt -> let fmt = unwrapRow fmt in
    let name = objName obj in
    let kind = objKind obj in
    let code = switch obj.kind
    case ObjLet { rec = rec, args = args, ty = ty } then
        let t = match ty with Some t then type2str t else "?" in
        let args = strJoin " " args in
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
        let t = match ty with Some t then type2str t else "?" in
        join ["sem ", name, " : ", t]
    case kind then
        join [getFirstWord kind, " ", name]
    end in
    renderSourceCodeStr code fmt

    sem renderGotoLink (link: String) =
    | _ -> "[→]"
        
    sem renderLinkList (objects: [Object]) =
    | fmt -> let fmt = unwrapRow fmt in
        let doc = map (lam u. renderLink (objTitle u) (objLink u) fmt) objects in
        let doc = strJoin ", " doc in
        match doc with "" then "" else
            concat (renderText doc fmt) (renderNewLine fmt)
    
    sem renderCodeWithoutPreview (data: RenderingData) = 
    | fmt -> let fmt = unwrapRow fmt in
        renderHidenCode (concat data.left data.right) false fmt

    sem renderCodeWithPreview (data: RenderingData) =
    | fmt -> let fmt = unwrapRow fmt in
        match data.right with [] then
            join [data.left, data.trimmed]
        else 
            join [data.left, renderHidenCode data.right true fmt, data.trimmed]

    sem renderHidenCode (code : String) (withPreview: Bool) =
    | _ -> ""

    sem renderSourceCodeStr (code: String) =
    | fmt -> let fmt = unwrapRow fmt in
         renderSourceCode (strToSourceCode code) fmt

    sem renderSourceCode (code: SourceCode) =
    | fmt -> let fmt = unwrapRow fmt in
        join (map (lam code. match code with Some code then renderWord code fmt else "") code)
    
    sem renderWord (word: SourceCodeWord) = 
    | fmt -> let fmt = match fmt with Row { fmt = fmt }  then fmt else fmt in
        let renderSkiped: [Token] -> String = lam skiped.
            join (map (lam s. renderWord ( { word = s, kind = CodeDefault {} } ) fmt) skiped) in

        switch word
        case { word = Include { content = content, skiped = skiped } } then
            join [renderKeyword "include" fmt, renderSkiped skiped, renderString (join ["\"", (renderRemoveForbidenChars content fmt), "\""]) fmt]    
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
                case CodeNumber {} then renderNumber
                case CodeDefault {} then renderDefault
                end       
            end) in
            let word = renderRemoveForbidenChars (lit word) fmt in
            renderer word fmt
        end

    sem renderTreeSourceCode (tree: [TreeSourceCode]) (obj : Object) =
    | fmt -> let fmt = unwrapRow fmt in
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

    sem renderHeader (obj : Object) (theme: Theme) =
    | _ -> ""

    sem renderFooter (obj : Object) =
    | _ -> ""

    sem renderSectionTitle (title: String) =
    | fmt -> let fmt = unwrapRow fmt in
        renderTitle 2 title fmt

    sem renderBold (text : String) =
    | _ -> text

    sem renderRemoveForbidenChars (s: String) =
    | _ -> s


    sem renderTitle (size : Int) (s : String) =
    | _ -> s

    sem renderObjTitle (size : Int) (obj : Object) =
    | fmt -> let fmt = unwrapRow fmt in
        renderTitle size (objTitle obj) fmt
    
    sem renderText (text : String) =
    | _ -> text

    sem renderLink (title : String) (link : String) =
    | _ -> join [title, " (", link, ")"]
    
    sem renderType (content : String) = 
    | _ -> content

    sem renderVar (content : String) =
    | _ -> content
    
    sem renderKeyword (content : String) =
    | _ -> content
    
    sem renderComment (content : String) =
    | _ -> content
    
    sem renderString (content : String) =
    | _ -> content

    sem renderNumber (content : String) =
    | _ -> content
    
    sem renderDefault (content : String) =
    | _ -> content
    
    sem renderWeakComment (content : String) =
    | _ -> content

    sem renderNewLine =
    | _ -> "\n"
    
end
