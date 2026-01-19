-- # Rendering Options
--
-- Throughout the rendering process, we keep track of a `RenderingOptions` object.  
-- This structure stores essential information that controls how documentation is generated.  

include "./../global/format-language.mc"
include "./../global/format.mc"
include "../naming/name-context.mc"

include "./rendered-map.mc"

-- ## RenderingOptions
--
-- The configuration object passed around during rendering.
type RenderingOptions = use Formats in use FormatLanguages in
    {
        fmt: Format,
        stdlibFolder: String,
        outputFolder: String,
        srcFolder: String,
        urlPrefix: String, 
        fmtLang: FormatLanguage, 
        letDepth: Option Int, 
        nameContext: NameContext,
        noCode: Bool,
        renderedMap: RenderedMap,
        log: Logger
    }

let renderingOptionsSrcPath : RenderingOptions -> String =
    lam opt. normalizePath (join [opt.outputFolder, "/", opt.srcFolder])


-- Ensure RenderingOptions uses the wrapped (non-raw) format.
let fixOptFormat : RenderingOptions -> RenderingOptions = lam opt. { opt with fmt = use Formats in unwrapRaw opt.fmt }
