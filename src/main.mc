include "extracting/extracter.mc"
include "rendering/renderer.mc"
include "options.mc"
        
        
mexpr

    let opt = parseOptions argv in
    
    switch parse opt.file
    case Some result then
        -- displayTree result
        let obj = extract result in
        render obj
    case None {} then usage ()
    end
