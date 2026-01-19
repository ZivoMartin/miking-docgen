-- # MDX Renderer for mi-doc-gen
--
-- This module implements the **MdxRenderer**, an instance of `RendererInterface`.
-- It outputs MDX pages compatible with Docusaurus, delegating text formatting to
-- the Markdown renderer where appropriate, and using shared MDX components.
--
-- ## Design
-- - Writes a reusable MDX components file (`MikingDocGen.tsx/.jsx`) at setup.
-- - Renders headings/markdown via the Markdown renderer to keep escaping consistent.
-- - Builds semantic blocks (`DocBlock`, `ToggleWrapper`, etc.) in MDX.
-- - Trims trailing comment/blank lines from raw code when rendering code blocks.
-- - Removes `.md` from links so Docusaurus routes match clean URLs.
-- - Search bar here is still in todo.

include "./renderer-interface.mc"
include "./headers/mdx-components.mc"
include "../util.mc"

include "sys.mc"

let componentFileName = "MikingDocGen"
let searchFileName = searchPath ""

-- Provides the MDX renderer implementation and its dispatch rules.
lang MdxRenderer = RendererInterface

    -- Build an absolute path "<folder>/<name>.<ext>" for the components file.
    sem getComponentPath : FormatLanguage -> String -> String -> String
    sem getComponentPath =
    | fmtLang -> lam path. lam name.
        let path = if not (strEndsWith "/" path) then concat path "/" else path in
        let ext = concat "." (formatLanguageGetExt fmtLang) in
        let name = concatIfNot name (strEndsWith ext) ext in
        concat path name

    sem renderSearchFile (searchDatas: [SearchDictObj]) = 
    | { fmt = Mdx {} } & opt ->
        let path = getComponentPath (Js {}) (renderingOptionsSrcPath opt) searchFileName in
        let content = searchReact searchDatas in
        renderFileOrWarn path content

    -- Create the MDX components file (TSX/JSX) in the output folder.
    sem renderSetup =
    | { fmt = Mdx {} } & opt ->
        let srcPath = renderingOptionsSrcPath opt in
        let path = getComponentPath opt.fmtLang srcPath componentFileName in
        let components = match opt.fmtLang with Ts {} then mdxTsComponents else mdxJsComponents in
        renderFileOrWarn path components

    -- Emit import line for MDX components used by the page.
    sem renderHeader obj =
    | { fmt = Mdx {} } & opt ->
        let formatPath = lam full.
            let importPath = if strStartsWith "/" full
                             then subsequence full 1 (length full)
                             else full in
            --  strip .tsx/.js extension from the import path if present
            let path = if strEndsWith ".tsx" importPath
            then subsequence importPath 0 (subi (length importPath) 4)
            else if strEndsWith ".jsx" importPath
            then subsequence importPath 0 (subi (length importPath) 4)
            else importPath in
            normalizePath path
        in
            

        let path = normalizePath (join [opt.urlPrefix, "/", opt.srcFolder]) in
        let import = formatPath (getComponentPath opt.fmtLang path componentFileName) in
        let search = formatPath (getComponentPath (Js {}) path searchFileName) in
        
        join [
          "import { DocBlock, Signature, Description, ToggleWrapper, S} from '@site/", import, "';\n",
          "import Search from '@site/", search, "';\n\n",

          "<Search />\n"
        ]

    -- Reuse Markdown escaping for code.
    sem renderRemoveCodeForbidenChars (s: String) =
    | { fmt = Mdx {} } & opt -> renderRemoveCodeForbidenChars s { opt with fmt = Md {} }

    -- Reuse Markdown escaping for docs.
    sem renderRemoveDocForbidenChars (s: String) =
    | { fmt = Mdx {} } & opt -> renderRemoveDocForbidenChars s { opt with fmt = Md {} }

    -- Delegate headings to Markdown renderer.
    sem renderTitle size s =
    | { fmt = Mdx {} } & opt -> renderTitle size s { opt with fmt = Md {} }

    -- Delegate bold text to Markdown renderer.
    sem renderBold (text : String) =
    | { fmt = Mdx {} } & opt -> renderBold text { opt with fmt = Md {} }

    -- Delegate italic text to Markdown renderer.
    sem renderItalic (text : String) =
    | { fmt = Mdx {} } & opt -> renderItalic text { opt with fmt = Md {} }

    -- Delegate newline rendering to Markdown renderer ("  \n").
    sem renderNewLine =
    | { fmt = Mdx {} } & opt -> renderNewLine { opt with fmt = Md {} }

    -- Render object description as an MDX <Description> block (omit empty default).
    sem renderDocDescription desc =
    | { fmt = Mdx {} } & opt ->
      let desc = renderDocDescription desc { opt with fmt = Md {} } in
      let desc = if eqString desc "No documentation available here." then "" else desc in
      if eqString "" desc then "" else join ["<Description>\n", desc, "\n</Description>\n"]
        
    -- The goto link is directly handled in mdx component, so we always return empty string.
    sem renderGotoLink (link: String) =
    | { fmt = Mdx {} } & opt -> ""
    
    -- Render a single link, removing the trailing ".md" for Docusaurus routes.
    sem renderLink (title : String) (link : String) =
    | { fmt = Mdx {} } & opt ->
          let linkLength = length link in
          let link = subsequence link 0 (subi linkLength 3) in -- remove extension for Docusaurus
          join ["<a href={\"", link, "\"} style={S.link}>", title, "</a>"]
    
    -- Render a list of links by delegating to raw rendering, then add a newline.
    sem renderLinkList (objects: [Object]) =
    | { fmt = Mdx {} } & opt ->
        renderWithRaw opt "" renderLinkList objects (renderNewLine opt)

    -- Format a code string as a fenced block ```mc (with proper escaping).
    sem mdxRenderCode : RenderingOptions -> String -> String
    sem mdxRenderCode =
    | opt -> lam code.
      if null code then "" else
      join ["\n```mc\n", code, "\n```\n"]

    sem renderHidenCode (hidden: String) (shown: String) (code: String) (jumpLine: Bool) =
    | { fmt = Mdx {} } & opt ->
      let code = mdxRenderCode opt code in
      join ["<ToggleWrapper shownText=\"", shown, "\" hiddenText=\"", hidden, "\">", code, "</ToggleWrapper>", if jumpLine then "\n" else ""]

    -- Render signature via the raw MDX dispatcher; omit if empty.
    sem renderDocSignature (obj: Object) =
    | { fmt = Mdx {} } & opt -> 
        let code = renderWithRaw opt "" renderDocSignature obj "" in
        mdxRenderCode opt code 

    -- Render the full code (trim trailing comments/empties), escaped for MDX.
    sem renderCodeWithoutPreview (data: RenderingData) =
    | { fmt = Mdx {} } & opt ->
        let split = strSplit "\n" (renderingDataRaw data) in
        match splitOnR (lam l.
            let trimmed = strTrim l in
            not (or (strStartsWith "--" trimmed) (null trimmed))
        ) (reverse split) with (_, right)  in
        let code = strJoin "\n" (reverse right) in
        renderHidenCode "Show Implementation" "Hide Implementation" code true opt

    -- Render tests as raw text if available (panels are added by the caller).
    sem renderDocTests (data: RenderingData) =
    | { fmt = Mdx {} } & opt ->
        renderWithRaw opt "" renderDocTests data ""

    -- Render a full documentation block (title, signature, desc, code, optional tests).
    sem renderDocBloc (data: RenderingData) =
    | { fmt = Mdx {} } & opt ->
        let link = objGetMyLink data.obj opt in
        let linkLength = length link in
        let link = subsequence link 0 (subi linkLength 3) in -- remove extension for Docusaurus
        let link = if objHasUrl data.obj then join [" link=\"", link, "\""] else "" in 
        
        let title = objTitle data.obj in
        let form  = objGetFirstWord data.obj in
    
        let left = join ["<DocBlock title=\"", title, "\" form=\"", form, "\"", link, ">\n"] in
        let right = "</DocBlock>\n\n" in
        renderWithRaw opt left renderDocBloc data right
end
