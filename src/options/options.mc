-- # Command-Line Options Parser
--
-- This module provides parsing of command-line arguments
-- for the `my-doc-gen` documentation generator.
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
--   --url-prefix <prefix>                  Prefix for all generated URLs.
--   --depth <n|none>                       Limit nesting depth of `let` bindings.
--   --no-stdlib                            Do not include the standard library in the output.
--   --md-doc                               Generate Markdown documentation from inline comments.
--   --keep-tests-doc                       Keep inline documentation of tests.
--
-- Language Formatting:
--   --javascript                           Use JavaScript for the React components.
--   --typescript                           Use TypeScript for the React components.
--
-- Debugging Options:
--   --debug                                Enable all debug modes.
--   --no-warn                              Disable all warnings.
--
-- Help:
--   --help | --h                           Show this help message.
-- ```

include "../global/format.mc"
include "../global/format-language.mc"    
        
include "string.mc"
include "sys.mc"

-- ## Options
-- Data type representing the command-line options that can be passed to `my-doc-gen`.
type Options = use Formats in use FormatLanguages in {
    noOpen: Bool,            -- Whether to skip opening the result in a web browser.
    fmt: Format,             -- Output format (HTML, Markdown, MDX).
    fmtLang: FormatLanguage, -- Output language for generated React components (JS/TS).
    file: String,            -- Path to the input file.
    debug: Bool,             -- Enable debug mode.
    noWarn: Bool,            -- Suppress warnings.
    outputFolder: String,    -- Destination folder for generated output.
    noStdlib: Bool,          -- Whether to exclude the standard library.
    urlPrefix: String,       -- Prefix for generated URLs.
    letDepth: Option Int     -- Maximum nesting depth of let-bindings.
}

-- ## optionsDefault
-- Default values for the command-line options.
let optionsDefault : Options = use Formats in use FormatLanguages in {
    noOpen = false,
    fmt = defaultFormat (),
    fmtLang = defaultFormatLanguage (),
    file = "",
    debug = false,
    outputFolder = "doc-gen-output",
    noWarn = false,
    noStdlib = false,
    urlPrefix = "",
    letDepth = None {}
}

-- ## usage
-- Print usage instructions and terminate with an error.
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
    "  --url-prefix <prefix>                  Prefix for all generated URLs.\n",
    "  --depth <n|none>                       Limit nesting depth of `let` bindings.\n",
    "  --no-stdlib                            Do not include the standard library in output.\n",

    "Language Formatting:\n",
    "  --javascript                           Use JavaScript for the React components\n",
    "  --typescript                           Use TypeScript for the React components.\n\n",

    "Debugging Options:\n",
    "  --debug                                Enable all debug modes.\n",
    "  --no-warn                              Disable all warnings.\n",
 
    "Help:\n",
    "  --help | --h                           Show this help message.\n"
  ])

-- ## parseOptions
-- Parse the list of command-line arguments into an `Options` record.
-- Exits with an error if the arguments are invalid.
let parseOptions : [String] -> Options = lam argv.
    recursive let parse : [String] -> Options -> Options = use Formats in use FormatLanguages in lam args. lam opts.
        switch args
        case ["--help" | "--h"] then usage ()

        case ["--debug"] ++ rest then parse rest { opts with debug = true } 
        case ["--no-warn"] ++ rest then parse rest { opts with noWarn = true }

        case ["--javascript"] ++ rest then parse rest { opts with fmtLang = Js {} }
        case ["--typescript"] ++ rest then parse rest { opts with fmtLang = Ts {} }

        case ["--output-folder", outputFolder] ++ rest then parse rest { opts with outputFolder = outputFolder }
        case ["--url-prefix", urlPrefix] ++ rest then parse rest { opts with urlPrefix = urlPrefix }
        case ["--no-open"] ++ rest then parse rest { opts with noOpen = true }
 
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
