-- # Options Parser Module
--
-- This module parses command-line arguments for the doc generator.
--
-- Supported arguments:
--   --no-open             -> disables auto-opening the output in browser
--   --format <format>     -> sets output format (`html` or `md`), default is `html`
--   <file>                -> input file to process

-- Options type holding flags and input file path
type Options = {
    noOpen: Bool,    -- disable auto-open of output
    format: String,  -- output format: `html` or `md`
    file: String     -- input file path
}

-- Default options
let optionsDefault : Options = {
    noOpen = false,
    format = "html",
    file = ""
}

-- Usage message
let usage = lam x.
    error "Usage: <program> [--no-open] [--format <html|md>] <file>"

-- Parse command-line arguments into Options
let parseOptions : [String] -> Options = lam argv.
    -- Recursive helper to process args
    recursive let parse : [String] -> Options -> Options = lam args. lam opts.
        switch args
        case ["--no-open"] ++ rest then parse rest { opts with noOpen = true }
        case ["--format", "md" | "Markdown" | "markdown"] ++ rest then parse rest { opts with format = "md" }
        case ["--format", "html" | "Html" | "HTML" ] ++ rest then parse rest { opts with format = "html" }
        case [s] ++ rest then
            if eqString opts.file "" then parse rest { opts with file = s }
            else usage ()
        case [] then opts
        end
    in
    -- Skip program name (first argument), start parsing with defaults
    parse (tail argv) optionsDefault
