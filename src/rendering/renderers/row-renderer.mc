include "../source-code-spliter.mc"
include "./renderer-interface.mc"

lang RowRenderer = RendererInterface

    sem renderBlocDefault : RenderingData -> RenderingOptions -> String -> String -> String -> String -> String
    sem renderBlocDefault =
    | { obj = obj } & data -> lam opt. lam bonusTopDoc. lam bonusSignDescDoc. lam bonusDescCodeDoc. lam bonusBottomDoc.
        let signature = renderDocSignature obj opt in
        let description = renderDocDescription obj opt in
        let code = renderCodeWithoutPreview data opt in

        join [bonusTopDoc, signature, bonusSignDescDoc, description, bonusDescCodeDoc, code, bonusBottomDoc]

    sem fixOptFormat : RenderingOptions -> RenderingOptions
    sem fixOptFormat = | opt -> { opt with fmt = unwrapRow opt.fmt }
            
    sem renderTopPageDoc (data: RenderingData) =
    | opt -> let opt = fixOptFormat opt in
        let nl = renderNewLine opt in
        let details = switch data
        case { obj = { kind = ObjLang { parents = parents & ([_] ++ _) } } } then
            let parents = strJoin " + " (map (lam p. renderLink p (concat (getLangLink p) ".lang") opt) parents) in
            let sectionTitle = renderBold "Stem from:" opt in
            strJoin nl [sectionTitle, parents]
        case { obj = { kind = ( ObjSyn {} | ObjSem {} )} & obj } then
            let langName = objGetLangName obj in
            let langName = renderLink langName (concat (getLangLink langName) ".lang") opt in
            let sectionTitle = renderBold "From:" opt in
            strJoin nl [sectionTitle, langName]
        case { obj = obj } then
            ""
        end in
        renderBlocDefault data opt "" "" details ""
    
    sem renderDocBloc (data : RenderingData) =
    | opt -> let opt = fixOptFormat opt in
        match data with { obj = obj } in
        let link = objLink obj opt in
        let link = concat (if strStartsWith "/" link then "" else "/") link in
        let link = renderGotoLink link opt in
        renderBlocDefault data opt "" "" link ""
    
    sem renderDocDescription (obj: Object) =
    | opt -> let opt = fixOptFormat opt in
        let doc = objDoc obj in
        concat (renderRemoveForbidenChars doc opt) (renderNewLine opt)

    sem renderDocSignature (obj : Object) =
    | opt -> let opt = fixOptFormat opt in
        let type2str = lam t. strReplace "[Char]" "String" (type2str t) in
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
        renderSourceCodeStr code opt

    sem renderGotoLink (link: String) =
    | opt -> let opt = fixOptFormat opt in
        renderLink "[→]" link opt
        
    sem renderLinkList (objects: [Object]) =
    | opt -> let opt = fixOptFormat opt in
        let doc = map (lam u. renderLink (objTitle u) (objLink u opt) opt) objects in
        let doc = strJoin ", " doc in
        match doc with "" then "" else
            concat (renderText doc opt) (renderNewLine opt)
    
    sem renderCodeWithoutPreview (data: RenderingData) = 
    | opt -> let opt = fixOptFormat opt in
        renderHidenCode (concat data.left data.right) false opt

    sem renderCodeWithPreview (data: RenderingData) =
    | opt -> let opt = fixOptFormat opt in
        match data.right with [] then
            join [data.left, data.trimmed]
        else 
            join [data.left, renderHidenCode data.right true opt, data.trimmed]

    sem renderHidenCode (code : String) (withPreview: Bool) =
    | _ -> ""

    sem renderSourceCodeStr (code: String) =
    | opt -> let opt = fixOptFormat opt in
         renderSourceCode (strToSourceCode code) opt

    sem renderSourceCode (code: SourceCode) =
    | opt -> let opt = fixOptFormat opt in
        join (map (lam code. match code with Some code then renderWord code opt else "") code)
    
    sem renderWord (word: SourceCodeWord) = 
    | opt -> let opt = fixOptFormat opt in
        let renderSkiped: [Token] -> String = lam skiped.
            join (map (lam s. renderWord ( { word = s, kind = CodeDefault {} } ) opt) skiped) in

        switch word
        case { word = Include { content = content, skiped = skiped } } then
            join [renderKeyword "include" opt, renderSkiped skiped, renderString (join ["\"", (renderRemoveForbidenChars content opt), "\""]) opt]    
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
            let word = renderRemoveForbidenChars (lit word) opt in
            renderer word opt
        end

    sem renderTreeSourceCode (tree: [TreeSourceCode]) (obj : Object) =
    | opt -> let opt = fixOptFormat opt in
        match sourceCodeSplit tree with { left = left, right = right, trimmed = trimmed } in

        let renderSourceCode = lam b. renderSourceCode (wordBufferToSourceCode b) opt in
    
        let getFormatedString : [TreeSourceCode] -> String = lam code.
            foldl (lam s. lam node.
                concat (switch node 
                case TreeSourceCodeNode son then renderCodeWithPreview son opt
                case TreeSourceCodeSnippet code then renderSourceCode code
                end) s
                ) "" (reverse code) in

        let buildSourceCodeRow = lam code. join (map (lam w. lit w.word) code) in
        let row = foldl (lam row. lam tree.
             concat (switch tree 
                case TreeSourceCodeNode son then son.row
                case TreeSourceCodeSnippet code then buildSourceCodeRow code
                end) row)
                "" (reverse (concat left right)) in
        let row = concat row (match trimmed with TrimmedNotFormated code then buildSourceCodeRow code else "") in
    
        {
            obj = obj,
            left = getFormatedString left,
            right = getFormatedString right,
            trimmed = switch trimmed
                case TrimmedFormated s then s
                case TrimmedNotFormated b then renderSourceCode b
                end,
            row = row
        }

    sem renderHeader (obj : Object) =
    | _ -> ""

    sem renderFooter (obj : Object) =
    | _ -> ""

    sem renderSectionTitle (title: String) =
    | opt -> let opt = fixOptFormat opt in
        renderTitle 2 title opt

    sem renderBold (text : String) =
    | _ -> text

    sem renderRemoveForbidenChars (s: String) =
    | _ -> s


    sem renderTitle (size : Int) (s : String) =
    | _ -> s

    sem renderObjTitle (size : Int) (obj : Object) =
    | opt -> let opt = fixOptFormat opt in
        renderTitle size (objTitle obj) opt
    
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
