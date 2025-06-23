include "extracting/extracter.mc"
include "rendering/renderer.mc"
include "options.mc"
include "sys.mc"        
include "server.mc"

mexpr

    let opt = parseOptions argv in
    
    switch parse opt.file
    case Some result then
        -- displayTree result
        let obj = extract result in
        render obj;
        match obj with ObjectNode { obj = obj } then
            if opt.noOpen then () else startServer obj
        else never
    case None {} then usage ()
    end
