-- # Command-Line DocGenOptions Parser
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
-- General DocGenOptions:
--   --no-open                              Do not open the result in a web browser.
--   --output-folder <name>                 Set the output folder (default: doc-gen-output).
--   --src-folder <name>                    Destination folder for src files relative to outputFolder
--   --format <html|md|mdx>                 Choose output format (default: html).
--   --url-prefix <prefix>                  Prefix for all generated URLs.
--   --depth <n|none>                       Limit nesting depth of `let` bindings.
--   --md-doc                               Generate Markdown documentation from inline comments.
--
-- language Formatting:
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

-- ## DocGenOptions
-- Data type representing the command-line options that can be passed to `my-doc-gen`.
type DocGenOptions = use Formats in use FormatLanguages in {
    noOpen: Bool,              -- Whether to skip opening the result in a web browser.
    fmt: Format,               -- Output format (HTML, Markdown, MDX).
    fmtLang: FormatLanguage,   -- Output language for generated React components (JS/TS).
    files: [String],           -- Path to the input files.
    debug: Bool,               -- Enable debug mode.
    outputFolder: String,      -- Destination folder for generated output.
    srcFolder: String,         -- Destination folder for src files relative to outputFolder.
    urlPrefix: String,         -- Prefix for generated URLs.
    letDepth: Option Int,      -- Maximum nesting depth of let-bindings.
    stdlibFolder: String,      -- Name of the folder in which we should store stdlib files.
    noCode: Bool,              -- If true, implementations will not appears on the output.
    scanOnly: Bool,            -- If true, we only do a scan and pretty print it.
    keepMd: Bool
}

-- Default values for the command-line options.
let docGenOptionsDefault : DocGenOptions = use Formats in use FormatLanguages in {
    noOpen = false,
    fmt = defaultFormat (),
    fmtLang = defaultFormatLanguage (),
    files = [],
    debug = false,
    outputFolder = "doc-gen-output",
    srcFolder = "/",
    urlPrefix = "",
    letDepth = Some 1,
    stdlibFolder = "Stdlib",
    noCode = false,
    scanOnly = false,
    keepMd = false
}

-- Print usage instructions and terminate with an error.
let usage = lam.
  error (join [
    "Usage:\n",
    "  my-doc-gen [options] <file>\n\n",

    "Required:\n",
    "  [<file>|<folder>]                      List of files/folders to document.\n\n",

    "General DocGenOptions:\n",
    "  --no-open                              Do not open the result in a web browser.\n",
    "  --output-folder <name>                 Set the output folder (default: doc-gen-output).\n",
    "  --src-folder <name>                    Destination folder for src files relative to outputFolder.\n",
    "  --format <html|md|mdx>                 Choose output format (default: html).\n",
    "  --url-prefix <prefix>                  Prefix for all generated URLs.\n",
    "  --depth <n|none>                       Limit nesting depth of `let` bindings.\n",
    "  --no-code                              If true, implementations will not appears on the output",
    "  --stdlib-loc <loc>                     Name of the folder in which we should store stdlib files.",
    "  --keep-md                              Will not escape the md characters during rendering.",

    "Language Formatting:\n",
    "  --javascript                           Use JavaScript for the React components\n",
    "  --typescript                           Use TypeScript for the React components.\n\n",

    "Debugging Options:\n",
    "  --debug                                Enable all debug modes.\n",
    "  --no-warn                              Disable all warnings.\n",
    "  --scan-only                            Only process the scan of the project and print it.\n",    
 
    "Help:\n",
    "  --help | --h                           Show this help message.\n"
  ])
