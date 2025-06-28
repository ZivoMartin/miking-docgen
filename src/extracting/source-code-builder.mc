-- # Code Source Reconstruction Utilities
--
-- This module implements a minimalist system to reconstruct the source code
-- associated with each `Object` in the documentation tree.
-- 
-- The core idea is that the parser structure preserves the order of sub-blocks,
-- so we can mark recursive children using `None {}` and inject them later during rendering.
-- No explicit indexing or backtracking is required.

include "../parsing/doc-tree.mc"

-- ## SourceCodeWord
--
-- Represents a single word from the source code.
-- - `Some word` -> an actual token string
-- - `None {}`   -> a placeholder for a child block's source code
type SourceCodeWord = Option String

-- Representation of the source code with word's buffer
type SourceCode = [SourceCodeWord]

-- An empty source code
let sourceCodeEmpty : () -> SourceCode = lam . []
        
-- ## SourceCodeBuilder
--
-- Accumulates source code tokens while traversing the `DocTree`.
-- When entering a `Node`, a new builder context is pushed.
-- When finishing a node, the buffer is returned and the parent context is restored.
type SourceCodeBuilder
con SourceCodeNode : { parent: SourceCodeBuilder, buffer: [SourceCodeWord] } -> SourceCodeBuilder
con SourceCodeRoot : { buffer: [SourceCodeWord] } -> SourceCodeBuilder
    
-- ## absorbWord
--
-- Adds a token from the `DocTree` to the current builder.
-- - If the word is a `Leaf`, it's added as `Some word`.
-- - If the word is a `Node`, we inject a `None {}` into the parent
--   and start a fresh buffer for the child node.
let absorbWord : SourceCodeBuilder -> DocTree -> SourceCodeBuilder =
    use TokenReader in lam builder. lam word.
    match builder with SourceCodeNode { buffer = buffer } | SourceCodeRoot { buffer = buffer } in
    switch word
    case Node { token = token } then
        let buffer = cons (None {}) buffer in
        let parent =
            match builder with SourceCodeNode { parent = parent} then
                SourceCodeNode { parent = parent, buffer = buffer}
            else 
                SourceCodeRoot { buffer = buffer} in
        SourceCodeNode { parent = parent, buffer = [Some (lit token)] }
    case Leaf { token = token } then
        let buffer = cons (Some (lit token)) buffer in
        match builder with SourceCodeNode { parent = parent} then
            SourceCodeNode { parent = parent, buffer = buffer }
        else 
            SourceCodeRoot { buffer = buffer}  
    end
-- ## finish
--
-- Completes the current builder scope and returns:
-- - the restored parent builder
-- - the reversed buffer containing the current block's source
let finish : SourceCodeBuilder -> { builder: SourceCodeBuilder, sourceCode: SourceCode } = lam builder.
    match builder with SourceCodeNode { parent = parent, buffer = buffer } in
        { builder = parent, sourceCode = buffer }

-- Returns a new SourceCodeBuilder
let newSourceCodeBuilder : () -> SourceCodeBuilder = lam . SourceCodeRoot { buffer = [] }    
