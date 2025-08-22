-- # Miking Doc Generator
--
-- **This is the main entry point for the Miking Doc Generator.**
--
-- The project is composed of the following modules:
-- - parsing: Builds a DocTree representing the structure of the file.
-- - extracting: Converts DocTree into an ObjectTree suitable for generating documentation.
-- - labeling: Annotates each objects with an enventual type using compiler's AST 
-- - rendering: Renders the `ObjectTree` into Markdown pages (HTML support planned).

include "./execution-context.mc"

mexpr
    let execCtx = execContextNew () in
    let execCtx = gen execCtx in
    let execCtx = parse execCtx in
    let execCtx = extract execCtx in
    let execCtx = label execCtx in
    let execCtx = render execCtx in
    let execCtx = serve execCtx in
    ()
