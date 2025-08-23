-- # Logger Module
--
-- This module defines a simple logging framework with support for namespaced
-- logging (e.g., `Parsing`, `Extracting`, `Rendering`) and conditional output
-- based on the debug flag.

include "util.mc"
include "../options/options.mc"
include "format.mc"

-- Global options parsed from the command line.
let logOpt = parseOptions argv

-- Print a log message with a given kind, namespace, and message.
let message : String -> String -> String -> () = lam kind. lam namespace. lam message.
    printLn (join [kind, " from ", namespace, ": ", message])
    
-- Display a warning message.
let warn : String -> String -> () = lam m1. lam m2. message "WARNING" m1 m2

-- Display an info message if debug mode is enabled.
let log : String -> String -> () = lam kind. lam m. if logOpt.debug then message "INFO" kind m else ()

let parsingLog : String -> () =  log "Parsing"
let extractingLog : String -> () = log "Extracting"
let renderingLog : String -> () = log "Rendering"
let labelingLog : String -> () = log "Labeling"

let parsingWarn : String -> () = warn "Parsing"
let extractingWarn : String -> () = warn "Extracting"
let renderingWarn : String -> () = warn "Rendering"
let labelingWarn : String -> () = warn "Labeling"

-- Print the main configuration options if debug mode is enabled.
let optLog : Options -> () = use Formats in lam opt.
    let msg = join [
"Running miking-doc-gen with\n",
"   noOpen: ", bool2string opt.noOpen, "\n",
"   fmt: ", formatToStr opt.fmt, "\n",
"   file: ", opt.file] in
    if opt.debug then log "Main" msg else ()

-- Shadowing private logOpt.
let logOpt = ()
