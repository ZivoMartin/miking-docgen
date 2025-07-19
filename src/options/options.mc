-- # Options Parser Module
--
-- This module parses command-line arguments for the doc generator.
--
-- ## Usage
--
-- ```
-- my-doc-gen [options] <file>
--
-- Required:
--   <file>                    Path to the Miking source file to process.
--
-- Options:
--   --no-open                              Do not open the result in a browser.
--   --debug                                Enable all debug modes.
--   --parsing-debug                        Enable debug logs for parsing stage.
--   --extracting-debug                     Enable debug logs for extracting stage.
--   --labeling-debug                       Enable debug logs for labeling stage.
--   --rendering-debug                      Enable debug logs for rendering stage.
--   --no-warn                              Disable all warnings.
--   --no-parsing-warn                      Disable parsing warnings.
--   --no-extracting-warn                   Disable extracting warnings.
--   --no-labeling-warn                     Disable labeling warnings.
--   --no-rendering-warn                    Disable rendering warnings.
--   --format <html|md|mdx>                 Output format. Default: html.
--   --output-folder <name>                 Output folder name. Default: doc-gen-output.
--   --no-gen                               Do not parse anything, use the output folder as is.
--   --skip-labeling                        Skip type computation during parsing.
--   --theme <dark|light|warm-dark|warm>    Output theme. Default: dark.
--   --no-stdlib                            Disable stdlib includes in output.
--   --url-prefix <prefix>                  The link of each files will be prefixed by this argument
-- ```

include "../global/format.mc"
include "../global/theme.mc"
        
include "string.mc"

type Options = use Formats in use Themes in {
    noOpen: Bool,
    fmt: Format,
    theme: Theme,    
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
    noGen: Bool,
    skipLabeling: Bool,
    noStdlib: Bool,
    urlPrefix: String
}

let optionsDefault : Options = use Formats in use Themes in {
    noOpen = false,
    fmt = defaultFormat (),
    theme = defaultTheme (),    
    file = "",
    debug = false,
    parsingDebug = false,
    extractingDebug = false,
    labelingDebug = false,
    renderingDebug = false,
    skipLabeling = false,
    outputFolder = "doc-gen-output",
    noGen = false,
    noParsingWarn = false,
    noExtractingWarn = false,
    noLabelingWarn = false,
    noRenderingWarn = false,
    noWarn = false,
    noStdlib = false,
    urlPrefix = ""
}

let usage = lam.
    error (join [
        "Usage:\n",
        "  my-doc-gen [options] <file>\n\n",
        "Required:\n",
        "  <file>                                 Path to the Miking source file to process.\n\n",
        "Options:\n",
        "  --no-open                              Do not open the result in a browser.\n",
        "  --debug                                Enable all debug modes.\n",
        "  --parsing-debug                        Enable debug logs for parsing stage.\n",
        "  --extracting-debug                     Enable debug logs for extracting stage.\n",
        "  --labeling-debug                       Enable debug logs for labeling stage.\n",
        "  --rendering-debug                      Enable debug logs for rendering stage.\n",
        "  --no-warn                              Disable all warnings.\n",
        "  --no-parsing-warn                      Disable parsing warnings.\n",
        "  --no-extracting-warn                   Disable extracting warnings.\n",
        "  --no-labeling-warn                     Disable labeling warnings.\n",
        "  --no-rendering-warn                    Disable rendering warnings.\n",
        "  --format <html|md>                     Output format. Default: html.\n",
        "  --output-folder <name>                 Output folder name. Default: doc-gen-output.\n",
        "  --no-gen                               Do not parse anything, use the output folder as is.\n",
        "  --skip-labeling                        Skip type computation during parsing.\n",
        "  --theme <dark|light|warm-dark|warm>    Output theme. Default: dark.\n",
        "  --url-prefix <prefix>                  The link of each files will be prefixed by this argument.\n"
    ])

let parseOptions : [String] -> Options = lam argv.
    recursive let parse : [String] -> Options -> Options = use Formats in use Themes in lam args. lam opts.
        switch args
        case ["--no-open"] ++ rest then parse rest { opts with noOpen = true }
        case ["--no-gen"] ++ rest then parse rest { opts with noGen = true }
        case ["--skip-labeling"] ++ rest then parse rest { opts with skipLabeling = true }

        case ["--debug"] ++ rest then parse rest { opts with debug = true }
        case ["--parsing-debug"] ++ rest then parse rest { opts with parsingDebug = true }
        case ["--extracting-debug"] ++ rest then parse rest { opts with extractingDebug = true }
        case ["--labeling-debug"] ++ rest then parse rest { opts with labelingDebug = true }       
        case ["--rendering-debug"] ++ rest then parse rest { opts with renderingDebug = true }

        case ["--no-warn"] ++ rest then parse rest { opts with noWarn = true }
        case ["--no-stdlib"] ++ rest then parse rest { opts with noStdlib = true }    
        case ["--no-parsing-warn"] ++ rest then parse rest { opts with noParsingWarn = true }
        case ["--no-extracting-warn"] ++ rest then parse rest { opts with noExtractingWarn = true }
        case ["--no-labeling-warn"] ++ rest then parse rest { opts with noLabelingWarn = true }       
        case ["--no-rendering-warn"] ++ rest then parse rest { opts with noRenderingWarn = true }                

        case ["--output-folder", outputFolder] ++ rest then parse rest { opts with outputFolder = outputFolder }

        case ["--format", fmt] ++ rest then
            match formatFromStr fmt with Some fmt then
                parse rest { opts with fmt = fmt }
            else usage ()

        case ["--theme", theme] ++ rest then
            match themeFromStr theme with Some theme then
                parse rest { opts with theme = theme }
            else usage ()

        case ["--url-prefix", urlPrefix] ++ rest then parse rest { opts with urlPrefix = urlPrefix }

        case [s] ++ rest then
            if eqString opts.file "" then parse rest { opts with file = s }
            else usage ()

        case [] then opts
        end
    in
    parse (tail argv) optionsDefault


let opt: Options = parseOptions argv
