-- # Mast Generator Module
--
-- This is the first step of the Miking Doc Gen pipeline.
-- Later in the pipeline, we need the complete Miking compiler AST to fetch
-- types and resolve ambiguity. This module generates the Miking compiler AST,
-- which we call the MAST.
--
-- Generating the MAST from a file is simple: call `parseMCoreFile` with the
-- right options. However, the Miking compiler drops `utests` and `mexpr` in
-- non-entry point files, but we need an MAST of the entire program.
--
-- To address this, we generate a temporary file using the `sys.mc` API,
-- combining all included files so that `utests` and `mexpr` remain. This
-- process has three challenges:
--
-- 1. We must remove all `include`s from this file, otherwise parsing fails
--    since the temporary file is in `/tmp`. This is handled with the
--    `file-opener` API.
-- 2. Miking syntax allows only one `mexpr`. We must process all files and
--    replace `mexpr` with `let #var"x" =`.
-- 3. The parsing step later needs the raw code, and re-opening files would be
--    wasteful. We therefore insert all file contents into the `include-set`.

include "mexpr/boot-parser.mc"
include "mexpr/keywords.mc"
include "ocaml/external.mc"
include "mexpr/type-check.mc"
include "mexpr/ast.mc"
include "mexpr/info.mc"
include "sys.mc"
include "ext/file-ext.mc"

include "./mast.mc"

include "../global/util.mc"
include "../global/logger.mc"

-- Builds the AST from a file using the Miking compiler parser.
let buildMAstFromFile: Logger -> String -> MAst = lam log. lam file.
    use MExprTypeCheck in
    use MExprSym in
    use BootParser in
    use TokenReader in

    let externalsExclude = mapKeys (externalGetSupportedExternalImpls ()) in
    let parseOpt = {{{{{{{ defaultBootParserParseMCoreFileArg
          with keepUtests = true }
          with allowFree = true }
          with pruneExternalUtests = false }
          with externalsExclude = externalsExclude }
          with pruneExternalUtestsWarning = true }
          with eliminateDeadCode = false }
          with keywords = mexprExtendedKeywords } in

    log "Parsing ast";

    let ast = parseMCoreFile parseOpt file in

    log "Symbolizing final ast";

    let ast = symbolize ast in

    log "Type checking final ast";

    let ast = typeCheckExpr { typcheckEnvDefault with disableConstructorTypes = true} ast in

    ast
