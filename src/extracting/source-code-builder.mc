-- # Code Source Reconstruction Utilities
--
-- This module implements a minimalist system to reconstruct the source code
-- associated with each `Object` in the documentation tree.
-- 
-- The core idea is that the parser structure preserves the order of sub-blocks,
-- so we can mark recursive children using `None {}` and inject them later during rendering.
-- No explicit indexing or backtracking is required.

include "../parsing/doc-tree.mc"
include "./source-code-word.mc"
include "./colorizer.mc"
include "../logger.mc"

-- Representation of the source code with word's buffer
type SourceCode = [Option SourceCodeWord]

-- An empty source code
let sourceCodeEmpty : () -> SourceCode = lam . []
        
-- ## SourceCodeBuilder
--
-- Accumulates source code tokens while traversing the `DocTree`.
-- When entering a `Node`, a new builder context is pushed.
-- When finishing a node, the buffer is returned and the parent context is restored.
type SourceCodeBuilder
con SourceCodeNode : use Colorizer in { parent: Option SourceCodeBuilder, ctx: ColorizerContext, buffer: SourceCode } -> SourceCodeBuilder
    
-- ## absorbWord
--
-- Adds a token from the `DocTree` to the current builder.
-- - If the word is a `Leaf`, it's added as `Some word`.
-- - If the word is a `Node`, we inject a `None {}` into the parent
--   and start a fresh buffer for the child node.
let absorbWord : SourceCodeBuilder -> DocTree -> SourceCodeBuilder =
    use TokenReader in use Colorizer in lam builder. lam word.
    match builder with SourceCodeNode { buffer = buffer, parent = parent, ctx = ctx } in
    let token = (match word with Node { token = token } | Leaf { token = token } | IncludeNode { token = token } in token) in
    let ctx = colorizerNext (ctx, token) in
    let token = ctx.word in
    switch word
    case Node {} then
        let buffer = cons (None {}) buffer in
        let parent = SourceCodeNode { parent = parent, buffer = buffer, ctx = ctx } in
        SourceCodeNode { parent = Some parent, buffer = [Some token], ctx = ctx }
    case Leaf {} | IncludeNode {} then
        let buffer = cons (Some token) buffer in
        SourceCodeNode { parent = parent, buffer = buffer, ctx = ctx }
    end

-- ## finish
--
-- Completes the current builder scope and returns:
-- - the restored parent builder
-- - the reversed buffer containing the current block's source
let finish : SourceCodeBuilder -> { builder: SourceCodeBuilder, sourceCode: SourceCode } = lam builder.
    match builder with SourceCodeNode { parent = Some parent, buffer = buffer, ctx = ctx } then
        match parent with SourceCodeNode { parent = parent, buffer = parentBuffer  } in
        { builder = SourceCodeNode { parent = parent, buffer = parentBuffer, ctx = ctx }, sourceCode = reverse buffer }
    else match builder with SourceCodeNode { buffer = buffer } in
        extractingWarn "finish: Builder parent should never be empty at this point";
        { builder = builder, sourceCode = reverse buffer }

-- Returns a new SourceCodeBuilder
let newSourceCodeBuilder : () -> SourceCodeBuilder = use Colorizer in lam . SourceCodeNode { buffer = [], parent = None {}, ctx = colorizerEmptyContext () }
