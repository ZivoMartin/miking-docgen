-- # Miking Doc Generator
--
-- **This is the main entry point for the Miking Doc Generator.**
--
-- The project is composed of the following modules:
-- - parsing: Builds a DocTree representing the structure of the file.
-- - extracting: Converts DocTree into an ObjectTree suitable for generating documentation.
-- - labeling: Annotates each objects with an enventual type using compiler's AST 
-- - rendering: Renders the `ObjectTree` into Markdown pages (HTML support planned).

include "./options.mc"
include "./parsing/parser.mc"
include "./extracting/extracter.mc"
include "./labeling/labeler.mc"
include "./rendering/renderer.mc"
include "./server.mc"

mexpr
    (if opt.noGen then () else
        logOpt opt;
        let tree = parseFile opt.file in
        let obj = extract tree in
        let obj = if opt.skipLabeling then obj else label obj in
        render { format = opt.fmt, theme = opt.theme, noStdlib = opt.noStdlib } obj);
    startServer ()
    
