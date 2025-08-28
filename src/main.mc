-- # Miking Doc Gen
--
-- This file defines the **main execution pipeline** for mi-doc-gen.
-- It chains together all major stages, starting from option parsing and file loading,
-- down to rendering and serving the generated documentation.
--
-- Execution pipeline:
--   execContextNew → gen → parse → extract → label → render → serve
--
-- Each stage transforms the `ExecutionContext` in sequence,
-- progressively enriching it until the final output is produced.
--
-- Each step is very well documented, so I encourage you to check out their modules for more information.

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
