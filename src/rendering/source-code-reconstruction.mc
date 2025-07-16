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
let reconstructSourceCode : Object -> SourceCode -> [RenderingData] -> Format -> [TreeSourceCode] = 
    lam obj. lam code. lam sons. lam wordRenderer. lam codeHider.
    
    let sons = filter (lam s. match s.obj.kind with ObjInclude {} then false else true) sons in
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
                    { tree = concat [TreeSourceCodeNode son, TreeSourceCodeSnippet (reverse a.buffer)] a.tree, sons = sons, buffer = [] }
            else
                renderingWarn "Son array should not be empty at this point";
                a
        end) { tree = [], sons = sons, buffer = [] } code in
    
    reverse (match tree.buffer with [] then tree.tree else cons (TreeSourceCodeSnippet (reverse tree.buffer)) tree.tree)
   
