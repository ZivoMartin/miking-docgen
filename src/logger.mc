-- # Logger Module
--
-- This module defines a small logging framework with support for namespaced logging
-- (e.g., `Parsing`, `Extracting`, `Rendering`) and conditional output based on debug flags.

include "util.mc"
include "options.mc"
include "format.mc"

let message : String -> String -> String -> () = lam kind. lam namespace. lam message. printLn (concatAll [kind, " from ", namespace, ": ", message])
    
-- Displays a warning message
let warn : String -> String -> () = message "WARNING"

-- Displays a info message
let log : String -> String -> () = message "INFO"

let parsingLog : String -> () =  lam m.
    if or opt.parsingDebug opt.debug then log "Parsing" m else ()
let extractingLog : String -> () = lam m.
    if or opt.extractingDebug opt.debug then log "Extracting" m else ()
let renderingLog : String -> () = lam m.
    if or opt.renderingDebug opt.debug then log "Rendering" m else ()

let parsingWarn : String -> () = warn "Parsing"
let extractingWarn : String -> () = warn "Extracting"
let renderingWarn : String -> () = warn "Rendering"


let logOpt : Options -> () = use Formats in lam opt.
    let msg = concatAll [
"Running miking-doc-gen with\n",
"   noOpen: ", bool2string opt.noOpen, "\n",
"   fmt: ", formatToStr opt.fmt, "\n",
"   file: ", opt.file] in
    if opt.debug then log "Main" msg else ()
