-- # Source code reconstruction
--
-- This module provides a function to generate `RenderingData` from:
-- - A source `Object`
-- - The source code as a list of optional `SourceCodeWord`s
-- - A list of precomputed child `RenderingData`
-- - A word-to-string renderer
-- - A code hider (used to preview/hide code blocks)
--
-- It transforms a linearly structured source code into structured, displayable text by:
-- - Grouping source words and injected child blocks into a tree
-- - Splitting the tree at semantically meaningful points (`=`, `:`)
-- - Formatting both sides into renderable strings

include "./renderer-interface.mc"
include "./source-code-spliter.mc"
include "../extracting/source-code-builder.mc"
include "../extracting/source-code-word.mc"    
include "./rendering-types.mc"


-- ## getRenderingData
--
-- Converts one object and its associated parsed source into a single `RenderingData` node.
-- It stitches together `SourceCodeWord`s and child `RenderingData` into a tree,
-- splits the tree, formats both parts, and returns a structured rendering object.
--
-- ### Parameters:
-- - `obj`: the current AST object
-- - `code`: list of `Option SourceCodeWord` (`Some` for tokens, `None` for block markers)
-- - `sons`: rendering results of the object’s children
-- - `wordRenderer`: function to convert a `SourceCodeWord` to string
-- - `codeHider`: renderer for child blocks
--
-- ### Returns:
-- - A `RenderingData` record with `left`, `right`, and `trimmed` segments as strings    
let getRenderingData : Object -> SourceCode -> [RenderingData] -> WordRenderer -> CodeHider ->RenderingData  = lam obj. lam code. lam sons. lam wordRenderer. lam codeHider.
    type Arg = {
        tree: [TreeSourceCode],
        sons: [RenderingData],
        buffer: [SourceCodeWord]
    } in
    let tree = foldl (lam a: Arg. lam word: Option SourceCodeWord.
        switch word
        case Some w then { a with buffer = cons w a.buffer}
        case None {} then
            match a.sons with [son] ++ sons then
                match a.buffer with [] then
                    { a with tree = cons (TreeSourceCodeNode son) a.tree, sons = sons }
                else
                    { tree = concat [TreeSourceCodeNode son, TreeSourceCodeSnippet a.buffer] a.tree, sons = sons, buffer = [] }
            else
                renderingWarn "Son array should not be empty at this point";
                a
        end) { tree = [], sons = sons, buffer = [] } code in
    let tree = reverse (match tree.buffer with [] then tree.tree else cons (TreeSourceCodeSnippet tree.buffer) tree.tree) in
    match sourceCodeSplit tree with { left = left, right = right, trimmed = trimmed } in

    let getFormatedStringFromWordBuffer : [SourceCodeWord] -> String = lam code.
        foldl (lam s. lam w. concat (wordRenderer w) s) "" (reverse code) in

    let getFormatedString : [TreeSourceCode] -> String = lam code.
        foldl (lam s. lam node.
            concat (switch node 
            case TreeSourceCodeNode son then getCodeWithPreview codeHider son
            case TreeSourceCodeSnippet code then getFormatedStringFromWordBuffer code
            end) s
            ) "" code in
    
    {
        obj = obj,
        left = getFormatedString left,
        right = getFormatedString right,
        trimmed = switch trimmed
            case TrimmedFormated s then s
            case TrimmedNotFormated b then getFormatedStringFromWordBuffer b
            end
    }
