include "./../global/format-language.mc"
include "./../global/theme.mc"
include "./../global/format.mc"
    
type RenderingOptions = use Formats in use Themes in use FormatLanguages in
    {
        theme: Theme,
        fmt: Format,
        noStdlib: Bool,
        outputFolder: String,
        urlPrefix: String,
        fmtLang: FormatLanguage,
        letDepth: Option Int
    }
