-- # Miking Doc Generator
--
-- **This is the main entry point for the Miking Doc Generator.**
--
-- The project is composed of the following modules:
-- - parsing: Builds a DocTree representing the structure of the file.
-- - extracting: Converts DocTree into an ObjectTree suitable for generating documentation.
-- - labeling: Annotates each objects with an enventual type using compiler's AST 
-- - rendering: Renders the `ObjectTree` into Markdown pages (HTML support planned).

include "./options/options.mc"
include "./options/cast-options.mc"    
include "./parsing/parser.mc"
include "./extracting/extracter.mc"
include "./labeling/labeler.mc"
include "./rendering/renderer.mc"
include "./server/server.mc"
include "./execution-context.mc"

mexpr
    let execCtx = execContextNew () in
    let execCtx = if opt.noGen then execCtx else
        logOpt opt;
        let execCtx = parseFile execCtx opt.file in
        let execCtx = extract execCtx in
        let execCtx = label execCtx in
        render (getRenderingOption ()) execCtx;
        execCtx in
    startServer (getServeOption ()) execCtx
    
