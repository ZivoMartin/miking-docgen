-- # Options parser module
-- This module parses command line arguments to extract options for a Miking program.
-- It supports:
--   - --no-open  : disables opening the output
--   - <file>     : specifies the input file
-- If no file is provided or invalid arguments are given, an error message is shown.

-- Options type holding flags and file path
type Options = {
    noOpen: Bool,   -- whether to disable opening the output
    file: String    -- input file path
}

-- Default options
let optionsDefault : Options = {
    noOpen = false,
    file = ""
}

-- Usage message displayed in case of invalid arguments
let usage = lam x.
    error "Usage: <program> [--no-open] <file>"

-- Parse command line arguments into Options
let parseOptions : [String] -> Options = lam argv.
    -- Recursive helper function to process arguments
    recursive let parse : [String] -> Options -> Options = lam args. lam opts.
        switch args
        -- Case: "--no-open" flag
        case ["--no-open"] ++ rest then
            parse rest { opts with noOpen = true }

        case [s] ++ rest then
            if eqString opts.file "" then
                parse rest { opts with file = s }
            else
                usage ()

        case [] then
            opts
        end
    in
    -- Skip program name (first arg), start parsing with defaults
    parse (tail argv) optionsDefault
