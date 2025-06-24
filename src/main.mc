-- This is the main entry point for the Miking Doc Generator.
-- The project is composed of the following modules:
-- parsing/parser.mc: Builds a DocTree representing the structure of the file.
-- extracting/extracter.mc: Converts DocTree into an ObjectTree suitable for generating documentation.
-- rendering/renderer.mc: Renders the `ObjectTree` into Markdown pages (HTML support planned).

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
        render opt.fmt obj;
        match obj with ObjectNode { obj = obj } then
            if opt.noOpen then () else startServer obj opt.fmt
        else error "Extraction failed: `extract` should always return a tree with a Program root."
    case None {} then usage ()
    end
