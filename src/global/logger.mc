include "util.mc"
include "format.mc"

type Logger = String -> ()

-- Print a log message with a given kind, namespace, and message.
let message : String -> String -> Logger = lam kind. lam namespace. lam message.
    printLn (join [kind, " from ", namespace, ": ", message])

-- Display a warning message.
let warn : String -> Logger = lam m1. lam m2. message "[WARN]" m1 m2

let parsingWarn : Logger = warn "Parsing"
let labelingWarn : Logger = warn "Labeling"
let namingWarn : Logger = warn "Naming"
let renderingWarn : Logger = warn "Rendering"
let warn : Logger = lam w. printLn (concat "[WARN] " w)
