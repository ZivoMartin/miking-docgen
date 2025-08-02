include "./renderer-interface.mc"
include "./headers/mdx-components.mc"
    
include "sys.mc"

let componentFileName = "MikingDocGen"
    
lang MdxRenderer = RendererInterface

    sem getComponentPath : FormatLanguage -> String -> String -> String
    sem getComponentPath =
    | fmtLang -> lam path. lam name.
        let path = concatIfNot path (strEndsWith "/") "/" in
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
        let path = getComponentPath opt.fmtLang opt.urlPrefix componentFileName in
        join ["import { ToggleWrapper } from '@site/", path, "';\n\n"]
     
    sem renderTitle size s =
    | { fmt = Mdx {} } & opt -> renderTitle size s { opt with fmt = Md {} }

    sem renderBold (text : String) =
    | { fmt = Mdx {} } & opt -> renderBold text { opt with fmt = Md {} }

    sem renderNewLine =
    | { fmt = Mdx {} } & opt -> renderNewLine { opt with fmt = Md {} }

    sem renderRemoveCodeForbidenChars (s: String) =
    | { fmt = Mdx {} } & opt -> renderRemoveCodeForbidenChars s { opt with fmt = Md {} }

    sem renderRemoveDocForbidenChars (s: String) =
    | { fmt = Mdx {} } & opt -> renderRemoveDocForbidenChars s { opt with fmt = Md {} }

    sem renderDocDescription obj =
    | { fmt = Mdx {} } & opt -> renderDocDescription obj { opt with fmt = Md {} }
        
    sem renderGotoLink (link: String) =
    | { fmt = Mdx {} } & opt -> renderGotoLink link { opt with fmt = Md {} }
    
    sem renderLink (title : String) (link : String) =
    | { fmt = Mdx {} } & opt -> renderLink title link { opt with fmt = Md {} }
    
    sem renderLinkList (objects: [Object]) =
    | { fmt = Mdx {} } & opt -> renderLinkList objects { opt with fmt = Md {} }

    sem mdxRenderCode :  String -> String
    sem mdxRenderCode =
    | code -> join ["\n```mc\n", code, "\n```  \n\n"]
    

    sem renderDocSignature (obj: Object) =
    | { fmt = Mdx {} } & opt ->
        let sign = renderDocSignature obj  { opt with fmt = Row { fmt = Mdx {} } } in
        let nl = renderNewLine opt in    
        match sign with "" then
            ""
        else
            mdxRenderCode sign

    sem renderHidenCode code withPreview =
    | { fmt = Mdx {} } & opt -> join ["\n<ToggleWrapper>", code, "</ToggleWrapper>\n"]    

    sem renderCodeWithoutPreview (data: RenderingData) =
    | { fmt = Mdx {} } & opt ->
        let split = strSplit "\n" data.row in
        match splitOnR (lam l.
            let trimmed = strTrim l in
            not (or (strStartsWith "--" trimmed) (eqString "" trimmed))
        ) (reverse split) with { right = right } in
        let row = strJoin "\n" (reverse right) in
        let row = renderRemoveCodeForbidenChars row opt in
        renderHidenCode (mdxRenderCode row) false opt
        

end
