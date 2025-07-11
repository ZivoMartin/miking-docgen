-- # Options Parser Module
--
-- This module parses command-line arguments for the doc generator.
--
-- ## Supported arguments:
-- - `--no-open`  
--    Disables automatic opening of the output in the browser.
--
-- - `--debug`  
--    Enables global debugging mode (enables all sub-debug flags).
--
-- - `--parsing-debug`, `--extracting-debug`, `--rendering-debug`  
--    Enables specific logging for internal stages.
--
-- - `--format <format>`  
--    Sets the output format. Supported values: `html`, `md`. Default is `html`.
--
-- - `<file>`  
--    The source file to process. Required as the last positional argument.
--
-- ## Usage
-- ```
-- my-doc-gen [options] <file>
--
-- Options:
--   --no-open
--   --debug
--   --parsing-debug
--   --extracting-debug
--   --rendering-debug
--   --format html|md
-- ```

include "./format.mc"

-- Represents parsed CLI options.
-- Contains all toggles, selected format, and the input file path.
type Options = use Formats in {
    noOpen: Bool,            -- Disable auto-open of output
    fmt: Format,             -- Output format: `html` or `md`
    file: String,            -- Input file path
    debug: Bool,             -- Global debug flag
    parsingDebug: Bool,      -- Debug flag for parsing stage
    extractingDebug: Bool,   -- Debug flag for extracting stage
    renderingDebug: Bool,    -- Debug flag for rendering stage
    outputFolder: String,     -- Output name for the folder
    noGen: Bool,              -- Do not parse anything and uses the output folder 
    noTypes: Bool,           -- Disable types computing during parsing
    noTypeColor: Bool        -- Disable the type colorizer
}

-- The default state of all CLI options before parsing.
let optionsDefault : Options = use Formats in {
    noOpen = false,
    fmt = defaultFormat (),
    file = "",
    debug = false,
    parsingDebug = false,
    extractingDebug = false,
    renderingDebug = false,
    noTypes = false,
    noTypeColor = false,
    outputFolder = "doc-gen-output",
    noGen = false
}

-- Displays an error message and terminates the program if the CLI arguments are invalid.
-- This is called whenever an unrecognized flag or invalid format is encountered.
let usage = lam x.
    error "Usage: <program> [--no-open] [--debug] [--<parsing|extracting|rendering>-debug] [--format <html|md>] <file>"

-- Parses a list of command-line arguments and builds a fully populated `Options` value.
let parseOptions : [String] -> Options = lam argv.
    -- Recursive helper to process args
    recursive let parse : [String] -> Options -> Options = use Formats in lam args. lam opts.
        switch args
        case ["--no-open"] ++ rest then parse rest { opts with noOpen = true }
        case ["--debug"] ++ rest then parse rest { opts with debug = true }
        case ["--parsing-debug"] ++ rest then parse rest { opts with parsingDebug = true }
        case ["--extracting-debug"] ++ rest then parse rest { opts with extractingDebug = true }    
        case ["--rendering-debug"] ++ rest then parse rest { opts with renderingDebug = true }
        case ["--no-types"] ++ rest then parse rest { opts with noTypes = true }
        case ["--no-type-color"] ++ rest then parse rest { opts with noTypeColor = true }
        case ["--no-gen"] ++ rest then parse rest { opts with noGen = true }
        case ["--output-folder", outputFolder] ++ rest then parse rest { opts with outputFolder = outputFolder }
        case ["--format", fmt] ++ rest then
            match formatFromStr fmt with Some fmt then
                parse rest { opts with fmt = fmt }
            else usage ()
        case [s] ++ rest then
            if eqString opts.file "" then parse rest { opts with file = s }
            else usage ()
        case [] then opts
        end
    in
    -- Skip program name (first argument), start parsing with defaults
    parse (tail argv) optionsDefault    


-- Global `Options` instance parsed from `argv`.
-- This value is used throughout the application to control rendering behavior.
let opt: Options = parseOptions argv
