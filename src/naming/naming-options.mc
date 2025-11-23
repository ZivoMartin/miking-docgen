include "../global/format.mc"

type NamingOptions = use Formats in use FormatLanguages in
    {
        fmt: Format, 
        urlPrefix: String
    }
