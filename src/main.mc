-- This is the main entry point for the Miking Doc Generator.
-- The project is composed of the following modules:
-- parsing: Builds a DocTree representing the structure of the file.
-- extracting: Converts DocTree into an ObjectTree suitable for generating documentation.
-- rendering: Renders the `ObjectTree` into Markdown pages (HTML support planned).

include "./parsing/parser.mc"
include "./extracting/extracter.mc"
include "./rendering/renderer.mc"
include "./options.mc"
include "./server.mc"

mexpr
    logOpt opt;

    switch parse opt.file
    case Some result then
                
        let obj = extract result in
        render opt.fmt obj;
        match obj with ObjectNode { obj = obj } then
            if opt.noOpen then () else startServer obj opt.fmt
        else error "Extraction failed: `extract` should always return a tree with a Program root."
    case _ then usage ()
    end
