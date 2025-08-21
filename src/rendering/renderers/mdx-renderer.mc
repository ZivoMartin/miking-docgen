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
          "import { DocBlock, Signature, Description, Panel, ToggleWrapper, S } from '@site/",
          importPath,
          "';\n\n"
        ]

    sem renderRemoveCodeForbidenChars (s: String) =
    | { fmt = Mdx {} } & opt -> renderRemoveCodeForbidenChars s { opt with fmt = Md {} }

    sem renderRemoveDocForbidenChars (s: String) =
    | { fmt = Mdx {} } & opt -> renderRemoveCodeForbidenChars s { opt with fmt = Md {} }

    sem renderTitle size s =
    | { fmt = Mdx {} } & opt -> renderTitle size s { opt with fmt = Md {} }

    sem renderBold (text : String) =
    | { fmt = Mdx {} } & opt -> renderBold text { opt with fmt = Md {} }

    sem renderNewLine =
    | { fmt = Mdx {} } & opt -> renderNewLine { opt with fmt = Md {} }

    sem renderDocDescription obj =
    | { fmt = Mdx {} } & opt ->
      let rawDesc = renderDocDescription obj { opt with fmt = Md {} } in
      let desc = if eqString rawDesc "No documentation available here." then "" else rawDesc in
      if eqString "" desc then "" else join ["<Description>{`", desc, "`}</Description>\n"]
        
    sem renderGotoLink (link: String) =
    | { fmt = Mdx {} } & opt -> renderGotoLink link { opt with fmt = Md {} }
    
    sem renderLink (title : String) (link : String) =
    | { fmt = Mdx {} } & opt ->
          join ["<a href={\"", link, "\"} style={S.link}>", title, "</a>"]
    
    sem renderLinkList (objects: [Object]) =
    | { fmt = Mdx {} } & opt ->
        let nl = renderNewLine opt in
        join [renderLinkList objects { opt with fmt = Row { fmt = Mdx {}} }, nl]

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
        if eqString data.rowTests "" then "" else strFullTrim data.rowTests

    sem renderTopPageDoc (data: RenderingData) =
    | { fmt = Mdx {} } & opt ->
        let rawDesc = renderDocDescription data.obj { opt with fmt = Md {} } in
        let desc = strTrim rawDesc in
        let desc = if eqString rawDesc "No documentation available here." then "" else rawDesc in
        let toggleCode = join ["<ToggleWrapper>", mdxRenderCode opt (renderCodeWithoutPreview data opt), "</ToggleWrapper>"] in
        join ["\n", desc, if eqString desc "" then "" else "\n\n", toggleCode]


    sem renderDocBloc (data: RenderingData) (displayGotoLink: Bool) =
    | { fmt = Mdx {} } & opt ->
        let sign = renderDocSignature data.obj opt in
        let code  = renderCodeWithoutPreview data opt in
        let tests = renderDocTests data opt in
        let desc = renderDocDescription data.obj opt in
 
        let hasTests = not (eqString tests "") in

        let link = objLink data.obj opt in
        let link = concat opt.urlPrefix link in
        let linkLength = length link in
        let link = subsequence link 0 (subi linkLength 3) in -- removing extension for docusaurus
        let link = if displayGotoLink then join [" link=\"", link, "\""] else "" in 
        
        let title = objTitle data.obj in
        let kind  = getFirstWord (objKind data.obj) in
    
        let ns = objNamespace data.obj in
        let codeId  = join ["code-", ns] in
        let testsId = join ["tests-", ns] in
    
        join [
          "<DocBlock title=\"", title, "\" kind=\"", kind, "\"", link, ">\n",
          mdxRenderCode opt sign, "\n",
          desc, "\n",
          "<Panel id=\"", codeId, "\" title=\"Code\">", mdxRenderCode opt code, "</Panel>\n",
          (if hasTests then join ["<Panel id=\"", testsId, "\" title=\"Tests\">", mdxRenderCode opt tests, "</Panel>\n"] else ""),
          "</DocBlock>\n\n"
        ]
end     
