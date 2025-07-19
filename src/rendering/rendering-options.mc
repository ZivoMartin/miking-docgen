
type RenderingOptions = use Formats in use Themes in
    {
        theme: Theme,
        fmt: Format,
        noStdlib: Bool,
        outputFolder: String,
        urlPrefix: String
    }
