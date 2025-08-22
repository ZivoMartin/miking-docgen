-- # Command-Line Options Parser
--
-- Parses command-line arguments for the `my-doc-gen` documentation generator.
--
-- ## Usage
--
-- ```
-- my-doc-gen [options] <file>
--
-- Required:
--   <file>                                 Path to the Miking source file to document.
--
-- General Options:
--   --no-open                              Do not open the result in a web browser.
--   --output-folder <name>                 Set the output folder (default: doc-gen-output).
--   --format <html|md|mdx>                 Choose output format (default: html).
--   --theme <dark|light|warm-dark|warm>    Choose output theme (default: dark).
--   --url-prefix <prefix>                  Prefix for all generated URLs.
--   --depth <n|none>                       Limit nesting depth of `let` bindings.
--   --no-stdlib                            Do not include the standard library in output.
--   --md-doc                               Generate Markdown documentation from inline comments.
--   --keep-tests-doc                       Keep inline documentation of tests.
--
-- Language Formatting:
--   --javascript                           Use Javascript for the React components
--   --typescript                           Use Typescript for the React components.
--
-- Debugging Options:
--   --debug                                Enable all debug modes.
--   --parsing-debug                        Enable debug logs for the parsing stage.
--   --extracting-debug                     Enable debug logs for the extraction stage.
--   --labeling-debug                       Enable debug logs for the labeling stage.
--   --rendering-debug                      Enable debug logs for the rendering stage.
--
-- Warning Control:
--   --no-warn                              Disable all warnings.
--   --no-parsing-warn                      Disable parsing warnings.
--   --no-extracting-warn                   Disable extraction warnings.
--   --no-labeling-warn                     Disable labeling warnings.
--   --no-rendering-warn                    Disable rendering warnings.
--
-- Help:
--   --help | --h                           Show this help message.
-- ```

include "../global/format.mc"
include "../global/theme.mc"
include "../global/format-language.mc"    
        
include "string.mc"
include "sys.mc"

type Options = use Formats in use Themes in use FormatLanguages in {
    noOpen: Bool,
    fmt: Format,
    theme: Theme,
    fmtLang: FormatLanguage,     
    file: String,
    debug: Bool,
    parsingDebug: Bool,
    extractingDebug: Bool,
    labelingDebug: Bool,
    renderingDebug: Bool,
    noParsingWarn: Bool,
    noExtractingWarn: Bool,
    noLabelingWarn: Bool,
    noRenderingWarn: Bool,
    noWarn: Bool,
    outputFolder: String,
    noStdlib: Bool,
    urlPrefix: String,
    letDepth: Option Int,
    keepTestsDoc: Bool,
    mdDoc: Bool
}

let optionsDefault : Options = use Formats in use Themes in use FormatLanguages in {
    noOpen = false,
    fmt = defaultFormat (),
    theme = defaultTheme (),
    fmtLang = defaultFormatLanguage (),
    file = "",
    debug = false,
    parsingDebug = false,
    extractingDebug = false,
    labelingDebug = false,
    renderingDebug = false,
    outputFolder = "doc-gen-output",
    noParsingWarn = false,
    noExtractingWarn = false,
    noLabelingWarn = false,
    noRenderingWarn = false,
    noWarn = false,
    noStdlib = false,
    urlPrefix = "",
    letDepth = None {},
    keepTestsDoc = false,
    mdDoc = false
}

let usage = lam.
  error (join [
    "Usage:\n",
    "  my-doc-gen [options] <file>\n\n",

    "Required:\n",
    "  <file>                                 Path to the Miking source file to document.\n\n",

    "General Options:\n",
    "  --no-open                              Do not open the result in a web browser.\n",
    "  --output-folder <name>                 Set the output folder (default: doc-gen-output).\n",
    "  --format <html|md|mdx>                 Choose output format (default: html).\n",
    "  --theme <dark|light|warm-dark|warm>    Choose output theme (default: dark).\n",
    "  --url-prefix <prefix>                  Prefix for all generated URLs.\n",
    "  --depth <n|none>                       Limit nesting depth of `let` bindings.\n",
    "  --no-stdlib                            Do not include the standard library in output.\n",
    "  --md-doc                               Generate Markdown documentation from inline comments.\n",
    "  --keep-tests-doc                       Keep inline documentation of tests.\n",

    "Language Formatting:\n",
    "  --javascript                           Use Javascript for the React components\n",
    "  --typescript                           Use Typescript for the React components.\n\n",

    "Debugging Options:\n",
    "  --debug                                Enable all debug modes.\n",
    "  --parsing-debug                        Enable debug logs for the parsing stage.\n",
    "  --extracting-debug                     Enable debug logs for the extraction stage.\n",
    "  --labeling-debug                       Enable debug logs for the labeling stage.\n",
    "  --rendering-debug                      Enable debug logs for the rendering stage.\n\n",

    "Warning Control:\n",
    "  --no-warn                              Disable all warnings.\n",
    "  --no-parsing-warn                      Disable parsing warnings.\n",
    "  --no-extracting-warn                   Disable extraction warnings.\n",
    "  --no-labeling-warn                     Disable labeling warnings.\n",
    "  --no-rendering-warn                    Disable rendering warnings.\n\n",

    "Help:\n",
    "  --help | --h                           Show this help message.\n"
  ])

let parseOptions : [String] -> Options = lam argv.
    recursive let parse : [String] -> Options -> Options = use Formats in use Themes in use FormatLanguages in  lam args. lam opts.
        switch args
        case ["--help" | "--h"] then usage ()

        case ["--no-open"] ++ rest then parse rest { opts with noOpen = true }

        case ["--debug"] ++ rest then parse rest { opts with debug = true }
        case ["--parsing-debug"] ++ rest then parse rest { opts with parsingDebug = true }
        case ["--extracting-debug"] ++ rest then parse rest { opts with extractingDebug = true }
        case ["--labeling-debug"] ++ rest then parse rest { opts with labelingDebug = true }       
        case ["--rendering-debug"] ++ rest then parse rest { opts with renderingDebug = true }

        case ["--keep-tests-doc"] ++ rest then parse rest { opts with keepTestsDoc = true }

        case ["--md-doc"] ++ rest then parse rest { opts with mdDoc = true }

        case ["--no-warn"] ++ rest then parse rest { opts with noWarn = true }
        case ["--no-stdlib"] ++ rest then parse rest { opts with noStdlib = true }    
        case ["--no-parsing-warn"] ++ rest then parse rest { opts with noParsingWarn = true }
        case ["--no-extracting-warn"] ++ rest then parse rest { opts with noExtractingWarn = true }
        case ["--no-labeling-warn"] ++ rest then parse rest { opts with noLabelingWarn = true }       
        case ["--no-rendering-warn"] ++ rest then parse rest { opts with noRenderingWarn = true }                

        case ["--javascript"] ++ rest then parse rest { opts with fmtLang = Js {} }
        case ["--typescript"] ++ rest then parse rest { opts with fmtLang = Ts {} }

        case ["--output-folder", outputFolder] ++ rest then parse rest { opts with outputFolder = outputFolder }
        case ["--url-prefix", urlPrefix] ++ rest then parse rest { opts with urlPrefix = urlPrefix }
 
        case ["--depth", letDepth] ++ rest then
            match letDepth with "none" then
                parse rest { opts with letDepth = None {} }
            else if stringIsInt letDepth then
                parse rest { opts with letDepth = Some (string2int letDepth) }
            else usage ()

        case ["--format", fmt] ++ rest then
            match formatFromStr fmt with Some fmt then
                parse rest { opts with fmt = fmt }
            else usage ()

        case ["--theme", theme] ++ rest then
            match themeFromStr theme with Some theme then
                parse rest { opts with theme = theme }
            else usage ()

        case [s] ++ rest then
            if eqString opts.file "" then
               if sysFileExists s then
                  parse rest { opts with file = s }
               else
                  error (join ["While parsing options: file", s, " does not exist."])
            else usage ()

        case [] then opts
        end
    in
    parse (tail argv) optionsDefault
