include "./renderer-interface.mc"
include "./headers/mdx-components.mc"

include "sys.mc"

let componentFileName = "MikingDocGen"

lang MdxRenderer = RendererInterface

    sem getComponentPath : FormatLanguage -> String -> String -> String
    sem getComponentPath =
    | fmtLang -> lam path. lam name.
        let path = if not (strEndsWith "/" path) then concat path "/" else path in
        let ext = concat "." (formatLanguageGetExt fmtLang) in
        let name = concatIfNot name (strEndsWith ext) ext in
        concat path name

    sem renderSetup =
    | { fmt = Mdx {} } & opt ->
        let path = getComponentPath opt.fmtLang opt.outputFolder componentFileName in
        match fileWriteOpen path with Some wc then
            let write = fileWriteString wc in
            let components = match opt.fmtLang with Ts {} then mdxTsComponents else mdxJsComponents in
            write components;
            fileWriteClose wc
        else
            renderingWarn "Failed to create components file."

    sem renderHeader obj =
    | { fmt = Mdx {} } & opt ->
        let full = getComponentPath opt.fmtLang opt.urlPrefix componentFileName in
        let importPath = if strStartsWith "/" full
                         then subsequence full 1 (length full)
                         else full in
        -- optionnel : enlever extension .tsx/.js si présente
        let importPath =
            if strEndsWith ".tsx" importPath
            then subsequence importPath 0 (subi (length importPath) 4)
            else if strEndsWith ".js" importPath
            then subsequence importPath 0 (subi (length importPath) 3)
            else importPath in
        join [
          "import { DocBlock, Signature, Description, Actions, ActionToggle, ActionCopy, ActionPermalink, Panel } from '@site/",
          importPath,
          "';\n\n"
        ]

    sem renderRemoveDocForbidenChars (s: String) =
    | { fmt = Mdx {} } & opt ->
        strReplace "`" "\'" s

    sem renderRemoveCodeForbidenChars (s: String) =
    | { fmt = Mdx {} } & opt ->
        strReplace "`" "\'" s    

    sem renderTitle size s =
    | { fmt = Mdx {} } & opt -> renderTitle size s { opt with fmt = Md {} }

    sem renderBold (text : String) =
    | { fmt = Mdx {} } & opt -> renderBold text { opt with fmt = Md {} }

    sem renderNewLine =
    | { fmt = Mdx {} } & opt -> renderNewLine { opt with fmt = Md {} }

    sem renderDocDescription obj =
    | { fmt = Mdx {} } & opt -> renderDocDescription obj { opt with fmt = Md {} }
        
    sem renderGotoLink (link: String) =
    | { fmt = Mdx {} } & opt -> renderGotoLink link { opt with fmt = Md {} }
    
    sem renderLink (title : String) (link : String) =
    | { fmt = Mdx {} } & opt -> renderLink title link { opt with fmt = Md {} }
    
    sem renderLinkList (objects: [Object]) =
    | { fmt = Mdx {} } & opt -> renderLinkList objects { opt with fmt = Md {} }

    sem mdxRenderCode : RenderingOptions -> String -> String
    sem mdxRenderCode =
    | opt -> lam code. join ["\n```mc\n", renderRemoveCodeForbidenChars code opt, "\n```\n"]

    sem renderDocSignature (obj: Object) =
    | { fmt = Mdx {} } & opt -> 
        let sign = renderDocSignature obj { opt with fmt = Row { fmt = Mdx {} } } in
        if eqString sign "" then "" else sign

    sem renderCodeWithoutPreview (data: RenderingData) =
    | { fmt = Mdx {} } & opt ->
        let split = strSplit "\n" data.row in
        match splitOnR (lam l.
            let trimmed = strTrim l in
            not (or (strStartsWith "--" trimmed) (eqString "" trimmed))
        ) (reverse split) with { right = right } in
        let row = strJoin "\n" (reverse right) in
        renderRemoveCodeForbidenChars row opt

    sem renderDocTests (data: RenderingData) =
    | { fmt = Mdx {} } & opt ->
        if eqString data.rowTests "" then "" else data.rowTests

    sem renderTopPageDoc (data: RenderingData) =
    | { fmt = Mdx {} } & opt ->
        let rawDesc = renderDocDescription data.obj { opt with fmt = Md {} } in
        let descTrim = strTrim rawDesc in
        let desc = if eqString descTrim "No documentation available here." then "" else rawDesc in
        join [desc, if eqString desc "" then "" else "\n\n"]


    sem renderDocBloc (data: RenderingData) (displayGotoLink: Bool) =
    | { fmt = Mdx {} } & opt ->
        let sign = renderDocSignature data.obj opt in
        let rawDesc = renderDocDescription data.obj { opt with fmt = Md {} } in
        let descTrim = strTrim rawDesc in
        let desc = if eqString descTrim "No documentation available here." then "" else rawDesc in
    
        let code  = renderCodeWithoutPreview data opt in
        let tests = renderDocTests data opt in
        let hasTests = not (eqString tests "") in
    
        let title = objTitle data.obj in
        let href  = objLink data.obj opt in
        let kind  = getFirstWord (objKind data.obj) in
    
        let ns = objNamespace data.obj in
        let codeId  = join ["code-", ns] in
        let testsId = join ["tests-", ns] in
    
        let copyable = renderRemoveCodeForbidenChars data.row opt in
        let descBlock =
          if eqString desc "" then ""
          else join ["  <Description>{`", desc, "`}</Description>\n"] in
    
        join [
          "<DocBlock title=\"", title, "\" kind=\"", kind, "\" href=\"", href, "\">\n",
          mdxRenderCode opt sign, 
          descBlock,
          "  <Actions>\n",
          "    <ActionToggle target=\"", codeId, "\" labelShow=\"Display code\" labelHide=\"Hide code\" />\n",
          (if hasTests then join ["    <ActionToggle target=\"", testsId, "\" labelShow=\"Display tests\" labelHide=\"Hide tests\" />\n"] else ""),
          "    <ActionCopy code={`", copyable, "`} />\n",
          (if displayGotoLink then join ["    <ActionPermalink href=\"", href, "\" />\n"] else ""),
          "  </Actions>\n",
          "  <Panel id=\"", codeId, "\" title=\"Code\">", mdxRenderCode opt code, "</Panel>\n",
          (if hasTests then join ["  <Panel id=\"", testsId, "\" title=\"Tests\">", mdxRenderCode opt tests, "</Panel>\n"] else ""),
          "</DocBlock>\n\n"
        ]
end     
