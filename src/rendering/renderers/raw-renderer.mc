-- # Raw renderer
--
-- This file implements the raw renderer based on the raw format.
-- The raw format wraps another format. The wrapped format should be the
-- actual rendering target (though nothing prevents you from wrapping a
-- different format on purpose, resulting in hybrid outputs).
--
-- The core idea: implement general behavior here that always delegates by
-- passing the **wrapped** format as the argument—never the raw format itself.
-- This way, dispatch automatically reaches the correct implementation.

include "../source-code-spliter.mc"
include "./renderer-interface.mc"

lang RawRenderer = RendererInterface

    -- Runs before rendering all files (e.g., to generate global headers).
    sem renderSetup obj =
    | opt -> ()
    
    -- Default block renderer: composes signature, description, code, and tests.
    sem renderBlocDefault : RenderingData -> RenderingOptions -> String -> String -> String -> String -> String
    sem renderBlocDefault =
    | { obj = obj } & data -> lam opt. lam bonusTopDoc. lam bonusSignDescDoc. lam bonusDescCodeDoc. lam bonusBottomDoc.
        let opt = fixOptFormat opt in
        let signature = renderDocSignature obj opt in

        let doc = objDoc data.obj in
        let doc = renderDocObjectParse doc opt in
        let doc = renderFormattedDoc data.obj doc true opt in
        let doc = renderDocDescription doc opt in

        let code = if opt.noCode then "" else renderCodeWithoutPreview data opt in
        let tests = renderDocTests data opt in
        join [bonusTopDoc, signature, bonusSignDescDoc, doc, bonusDescCodeDoc, code, bonusBottomDoc, tests]
            
    -- Top page section: title + details (e.g., parent langs) + default block.
    sem renderTopPageDoc (data: RenderingData) =
    | opt -> let opt = fixOptFormat opt in
        let nl = renderNewLine opt in

        let renderStemFrom = lam obj. lam from.
            let link = renderSourceCodeStr from (Some obj) opt in -- Will cast into a single hook
            let sectionTitle = renderBold "From:" opt in
            strJoin nl [sectionTitle, link, ""]
        in

        let details = switch data
        case { obj = { kind = ObjLang { parents = parents & ([_] ++ _) } } & obj } then
            let parents = strJoin " + " (map (lam p. renderSourceCodeStr p (Some obj) opt) parents) in
            let sectionTitle = renderBold "Stem from:" opt in
            strJoin nl [sectionTitle, parents, ""]
        case { obj = { kind = ObjType {} } & obj } then
             renderTypeConstructors obj opt
        case { obj = { kind = ObjCon { parentType = parentType } } & obj } then
             renderStemFrom obj parentType
        case { obj = { kind = ObjSyn { variants = variants } } & obj } then
             let stemFrom = renderStemFrom obj (objGetLangName obj) in
             let variants = renderSynVariants obj variants opt in
             join [variants, nl, stemFrom]
        case { obj = { kind = ObjSem { variants = variants } } & obj } then
            renderStemFrom obj (objGetLangName obj)
        case { obj = obj } then
            ""
        end in
        renderBlocDefault data opt "" "" details ""

    sem renderSearchFile (searchDatas: [SearchDictObj]) =
    | opt -> let opt = fixOptFormat opt in
        let path = renderGetSearchPath opt in
        match fileWriteOpen path with Some wc then
              fileWriteString wc (searchReact searchDatas);
              fileWriteClose wc
        else
              renderingWarn (concat "Failed to create search file: " path)
    
    sem renderGetSearchPath =
    | opt -> ""

    -- Documentation block (optionally includes a “goto” link).
    sem renderDocBloc (data : RenderingData) =
    | opt -> let opt = fixOptFormat opt in
        match data with { obj = obj } in
        let link =
            if objRenderIt obj then
                let link = objGetMyLink obj opt in
                let link = concat (if strStartsWith "/" link then "" else "/") link in
                renderGotoLink link opt
            else ""
        in

        let details =
            switch objKind obj
            case ObjSyn { variants = variants } then
                let variants = renderSynVariants obj variants opt in
                renderHidenCode "▶" "▼" variants true opt
            case ObjType {} then
                let cons = renderTypeConstructors obj opt in            
                if null cons then "" else renderHidenCode "▶" "▼" cons true opt
            case _ then ""
            end
        in
        renderBlocDefault data opt "" details link ""
    
    -- Renders the description text of an object (from obj.doc).
    sem renderDocDescription (desc: String) =
    | opt -> let opt = fixOptFormat opt in
        concat desc (renderNewLine opt)

    sem renderPureDocSignature (obj : Object) =
    | opt -> let opt = fixOptFormat opt in
        let type2str = lam t. type2str t in
        let name = objName obj in
        let kind = objKind obj in
        switch obj.kind
        case ObjLet { ty = ty } then
            let t = match ty with Some t then type2str t else "?" in
            join ["let ", name, " : ", t]
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
        end

    -- Renders the object signature as source code.
    sem renderDocSignature (obj : Object) =
    | opt -> let opt = fixOptFormat opt in
        let code = renderPureDocSignature obj opt in
        renderSourceCodeStr code (Some obj) opt

    -- Renders the unit tests section (hidden if empty).
    sem renderDocTests (data: RenderingData) =
    | opt -> let opt = fixOptFormat opt in
        let tests = strFullTrim data.tests in
        if null tests then ""
        else renderHidenCode "Show Tests" "Hide Tests" tests true opt
    
    sem renderTypeConstructors (obj: Object) =
    | opt -> let opt = fixOptFormat opt in
        match nameContextGetTypeConstructors opt.nameContext obj with Some constructors then
            strJoin (renderNewLine opt)
                (map (lam cons.
                 let name = objName cons in
                 match objKind cons with ObjCon { t = t } then
                     let right = join [name, " ", t] in
                     let right = strToSourceCode right in
                     let right = renderSourceCode right (Some cons) opt in
                     let doc = objTryGetDoc cons in
                     let doc = strTrim doc in
                     if null doc then right else join [right, ": ", doc]
                 else renderingWarn "A constructor were expected here."; "")
                 constructors)
        else renderingWarn (join ["Failed to render constructors of ", objName obj, "."]); ""

    sem renderSynVariants (obj: Object) (variants: [SynVariant]) =
    | opt -> let opt = fixOptFormat opt in
        strJoin (renderNewLine opt)
                (map (lam v.
                 let right = join [v.name, " ", v.vtype] in
                 let right = strToSourceCode right in
                 let right = renderSourceCode right (Some obj) opt in
                 if null v.doc then right else join [right, ": ", v.doc])
                 variants)

    -- Goto link wrapper (uses renderLink).
    sem renderGotoLink (link: String) =
    | opt -> let opt = fixOptFormat opt in
        renderLink "[→]" link opt

    sem renderHookLink (title: String) (link: String) =
    | opt -> let opt = fixOptFormat opt in
        renderLink title link opt

    sem renderPageLink (title: String) (link: String) =
    | opt ->  let opt = fixOptFormat opt in
        renderLink title link opt

    -- Goto link wrapper (uses renderLink).
    sem renderParentLink (obj: Object) =
    | opt -> let opt = fixOptFormat opt in
        let namespace = objNamespace obj in
        let subnamespace = namespaceGetSubNamespace namespace in
        if namespaceIsRoot namespace then ""
        else match namespaceLast subnamespace with Some parentName then
              let link =
                  if strEndsWith ".mc" parentName then
                     buildUrl opt.stdlibFolder opt.urlPrefix opt.fmt (objIsStdlib obj) subnamespace
                  else
                    let parentName =
                        match strSplitOnce parentName '-' with Some { right = right } then right
                        else parentName
                    in
                    objGetLink obj opt parentName
              in
              if null link then "" else renderLink "←" link opt
        else ""


    -- Renders a comma-separated list of links for objects (with newline).
    sem renderLinkList (objects: [Object]) =
    | opt -> let opt = fixOptFormat opt in
        let doc = map (lam u.
            let link = objGetLink u opt (objName u) in
            renderPageLink (objTitle u) link opt
            ) objects
        in
        let doc = strJoin ", " doc in
        match doc with "" then "" else
            concat (renderText doc opt) (renderNewLine opt)
    
    -- Renders code as a hidden, toggleable block (raw + preview-less).
    sem renderCodeWithoutPreview (data: RenderingData) = 
    | opt -> let opt = fixOptFormat opt in
        renderHidenCode "Show Implementation" "Hide Implementation" (concat data.left data.right) true opt

    -- Renders code with an optional preview section (uses renderHidenCode).
    sem renderCodeWithPreview (data: RenderingData) =
    | opt -> let opt = fixOptFormat opt in
        match data.right with [] then
            join [data.left, data.trimmed]
        else 
            join [data.left, renderHidenCode "..." "..." data.right false opt, data.trimmed]

    -- Default hidden-code renderer (no-op for raw).
    sem renderHidenCode (hidden: String) (shown: String) (code : String) (jumpLine: Bool) =
    | _ -> ""

    -- String → tokenized/colored source code (delegates to renderSourceCode).
    sem renderSourceCodeStr (code: String) (obj: Option Object) =
    | opt -> let opt = fixOptFormat opt in
         renderSourceCode (strToSourceCode code) obj opt

    -- SourceCode → rendered string (maps each word with renderWord).
    sem renderSourceCode (code: SourceCode) (obj: Option Object) =
    | opt -> let opt = fixOptFormat opt in
        join (map (lam code. match code with Some code then renderWord code obj opt else "") code)
    
    -- Renders a single token/word according to its kind (with escaping).
    sem renderWord (word: SourceCodeWord) (obj: Option Object) =
    | opt -> let opt = fixOptFormat opt in
        let renderSkiped: [Token] -> String = lam skiped.
            join (map (lam s. renderWord ( { word = s, kind = CodeDefault {} } ) obj opt) skiped)
        in

        switch word
        case { word = TokenInclude { content = content, skiped = skiped } } then
            join [renderKeyword "include" opt, renderSkiped skiped, renderString (join ["\"", (renderRemoveCodeForbidenChars content opt), "\""]) opt]    
        case { word = word, kind = kind } then
            let renderer = (
            let lit = lit word in
            switch word
            case TokenStr {} then renderString
            case TokenMultiLineComment {} then renderMultiLineComment
            case TokenComment {} then renderComment
            case _ then
                switch kind
                case CodeKeyword {} then renderKeyword
                case CodeName {} then renderVar
                case CodeType {} then (lam word.
                                      let word = match strSplitOnce word '_' with Some { left = left, right = word } then word else word in
                                      let word =
                                          match obj with Some obj then renderHook obj word opt
                                          else word
                                      in
                                      renderType word)
                case CodeNumber {} then renderNumber
                case CodeDefault {} then renderDefault
                end       
            end) in
            let word = lit word in
            let word = renderRemoveCodeForbidenChars word opt in
            renderer word opt
        end

    -- Top-level source code rendering: splits, renders, and aggregates.
    -- If row s length is length than 30, we concatenate everything in left.
    sem renderTreeSourceCode (tree: [TreeSourceCode]) (tests: [RenderingData]) (obj : Object) =
    | opt -> let opt = fixOptFormat opt in
        match sourceCodeSplit tree with { left = left, right = right, trimmed = trimmed } in
        let renderSourceCode = lam b. renderSourceCode (wordBufferToSourceCode b) (None {}) opt in

        let integrateTests: RenderingData -> [RenderingData] -> RenderingData =
            lam d. lam tests.
            let testsStr: (String, String) =
                let name = objName obj in
                let tests = reverse tests in
                let tests = filter (lam t.
                    strContains name t.row
                 ) tests in
                match tests with [last] ++ tests then
                    let lastRow = last.row in
                    recursive let trimRow = lam row.
                      match row with [h] ++ t then
                            let l = strTrim h in
                            if strStartsWith "--" l then
                               trimRow t
                            else if eqString l "" then
                               trimRow t
                            else strJoin "\n" (reverse row)
                      else []
                    in
                    let lastRow = trimRow (reverse (strSplit "\n" lastRow)) in
                    
                    let row: String = join (map (lam t. t.row) (reverse tests)) in                            
                    let tests: String = join (map (lam t. join [t.left, t.right, t.trimmed]) (reverse tests)) in
                    (join [tests, last.left, last.right], concat row lastRow)
                else ("", "")
            in
            { d with tests = testsStr.0, rowTests = testsStr.1 }
        in

        let getFormatedString : [TreeSourceCode] -> String = lam code.
            foldl (lam s. lam node.
                concat (switch node 
                case TreeSourceCodeNode child then renderCodeWithPreview child opt
                case TreeSourceCodeSnippet code then renderSourceCode code
                end) s
                ) "" (reverse code) in

        let buildSourceCodeRaw = lam code. join (map (lam w. lit w.word) code) in
        let row = foldl (lam row. lam tree.
             concat (switch tree 
                case TreeSourceCodeNode child then child.row
                case TreeSourceCodeSnippet code then buildSourceCodeRaw code
                end) row)
                "" (reverse (concat left right)) in
        let row = concat row (match trimmed with TrimmedNotFormated code then buildSourceCodeRaw code else "") in
    
        let res = {
            obj = obj,
            left = getFormatedString left,
            right = getFormatedString right,
            trimmed = switch trimmed
                case TrimmedFormated s then s
                case TrimmedNotFormated b then renderSourceCode b
                end,
            tests = "",
            rowTests = "",
            row = row
        } in
        let res =
            if gti 200 (length res.row) then
              { res with left = join [res.left, res.right, res.trimmed], right = "", trimmed = "" }
            else res
        in
        integrateTests res tests

    -- File-level wrappers
    sem renderHeader (obj : Object) =
    | opt -> let opt = fixOptFormat opt in
      renderParentLink obj opt

    sem renderFooter (obj : Object) =
    | _ -> ""

    -- Section titles and basic text formatting.
    sem renderSectionTitle (title: String) =
    | opt -> let opt = fixOptFormat opt in
        renderTitle 2 title opt

    sem renderBold (text : String) =
    | _ -> text

    sem renderItalic (text : String) =
    | _ -> text

    -- Escaping/sanitizing hooks for docs and code (no-op in raw).
    sem renderRemoveDocForbidenChars (s: String) =
    | _ -> s

     sem renderRemoveCodeForbidenChars (s: String) =
    | _ -> s

    -- Title helpers.
    sem renderTitle (size : Int) (s : String) =
    | _ -> s

    sem renderObjTitle (size : Int) (obj : Object) =
    | opt -> let opt = fixOptFormat opt in
        renderTitle size (objTitle obj) opt
    
    -- Text, link, and color helpers (raw → passthrough).
    sem renderText (text : String) =
    | _ -> text

    sem renderHook (obj: Object) (name: String) =
    | opt -> let opt = fixOptFormat opt in
         let getStdlibFile = lam s.
             let ext = formatGetExtension opt.fmt in
             { url = normalizePath (join ["/", opt.stdlibFolder, "/", s, ".", ext]), obj = None {} }
         in
         let datas =
             switch name
             case "Int" then getStdlibFile "int.mc"
             case "Bool" then getStdlibFile "bool.mc"
             case "String" then getStdlibFile "string.mc"
             case "Char" then getStdlibFile "char.mc"
             case _ then
                match objTryFetch obj opt name with Some datas then
                    { url = datas.url, obj = Some datas.obj }
                else if eqString (objName obj) name then
                    { url = objGetMyLink obj opt, obj = Some obj }
                else
                    { url = "", obj = None {} }
             end
         in  
         if null datas.url then name else
         let link = renderHookLink name datas.url opt in
         match datas.obj with Some obj then
             let doc = objTryGetDoc obj in
             let doc = strTrim doc in
             let doc = renderDocObjectParse doc opt in
             let doc = renderFormattedDoc obj doc false opt in
             let sign = renderPureDocSignature obj opt in
             let sign = renderSourceCodeStr sign (None {}) opt in
             let doc = join [sign, if null doc then "" else "\n\n", doc] in
             renderTooltip link doc opt
         else link


    sem renderLink (title : String) (link : String) =
    | _ -> join [title, " (", link, ")"]

    sem renderTooltip (title : String) (content : String) =
    | _ -> join [title, " (", content, ")"]

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
    
    sem renderMultiLineComment (content : String) =
    | _ -> content

    -- Newline helper.
    sem renderNewLine =
    | _ -> "\n"
    
end
