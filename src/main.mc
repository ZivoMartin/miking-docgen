-- This is the main entry point for the Miking Doc Generator.
-- The project is composed of the following modules:
-- parsing: Builds a DocTree representing the structure of the file.
-- extracting: Converts DocTree into an ObjectTree suitable for generating documentation.
-- rendering: Renders the `ObjectTree` into Markdown pages (HTML support planned).

include "./options.mc"
include "./parsing/parser.mc"
include "./extracting/extracter.mc"
include "./labeling/labeler.mc"
include "./rendering/renderer.mc"
include "./server.mc"
    
mexpr
    logOpt opt;
    let tree = parseFile opt.file in
    let obj = extract tree in
    let obj = label obj in
    render opt.fmt obj;
    startServer obj
    
