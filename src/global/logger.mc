-- # Logger Module
--
-- This module defines a small logging framework with support for namespaced logging
-- (e.g., `Parsing`, `Extracting`, `Rendering`) and conditional output based on debug flags.

include "util.mc"
include "../options/options.mc"
include "format.mc"

let _logOpt = parseOptions argv

let message : String -> String -> String -> () = lam kind. lam namespace. lam message. printLn (join [kind, " from ", namespace, ": ", message])
    
-- Displays a warning message
let warn : String -> String -> () = lam m1. lam m2.
    if _logOpt.noWarn then () else  message "WARNING" m1 m2

-- Displays a info message
let log : String -> String -> () = message "INFO"

let parsingLog : String -> () =  lam m.
    if or _logOpt.parsingDebug _logOpt.debug then log "Parsing" m else ()
let extractingLog : String -> () = lam m.
    if or _logOpt.extractingDebug _logOpt.debug then log "Extracting" m else ()
let renderingLog : String -> () = lam m.
    if or _logOpt.renderingDebug _logOpt.debug then log "Rendering" m else ()
let labelingLog : String -> () = lam m.
    if or _logOpt.labelingDebug _logOpt.debug then log "Labeling" m else ()

let parsingWarn : String -> () = lam m. if _logOpt.noParsingWarn then () else warn "Parsing" m
let extractingWarn : String -> () = lam m. if _logOpt.noExtractingWarn then () else warn "Extracting" m
let renderingWarn : String -> () = lam m. if _logOpt.noRenderingWarn then () else warn "Rendering" m
let labelingWarn : String -> () = lam m. if _logOpt.noLabelingWarn then () else warn "Labeling" m     


let optLog : Options -> () = use Formats in lam opt.
    let msg = join [
"Running miking-doc-gen with\n",
"   noOpen: ", bool2string opt.noOpen, "\n",
"   fmt: ", formatToStr opt.fmt, "\n",
"   file: ", opt.file] in
    if opt.debug then log "Main" msg else ()
