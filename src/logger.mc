include "util.mc"

let message : String -> String -> String -> () = lam kind. lam namespace. lam message. printLn (concatAll [kind, " from ", namespace, ": ", message])
    
-- Displays a warning message
let warn : String -> String -> () = message "WARNING"

-- Displays a warning message
let log : String -> String -> () = message "INFO"

let parsingLog : String -> () = log "Parsing"
let extractingLog : String -> () = log "Extracting"
let renderingLog : String -> () = log "Rendering"        

let parsingWarn : String -> () = log "Parsing"
let extractingWarn : String -> () = log "Extracting"
let renderingWarn : String -> () = log "Rendering"        
