-- # Rendering Types and Utilities
--
-- This module defines the core types and utilities used to represent and render source code for display purposes
--
-- It abstracts the notion of source code into a tree structure and enables rendering with or
-- without previews (e.g., collapsible code sections).

-- A function that converts a `SourceCodeWord` into its rendered string representation.
-- This can include syntax highlighting, escaping, or markup.
type WordRenderer = SourceCodeWord -> String

-- A function that takes a string (usually representing code) and returns a `hidden` version,
-- for example, collapsed HTML or a folded code block.
type CodeHider = String -> String
    
-- An abstract tree representation of source code. It supports:
type TreeSourceCode

-- This structure holds all the formatted code data related to a single source object.
--
-- - `left`: the code segment before the split point (e.g., `let x`)
-- - `right`: the code segment after the split (e.g., `= 42`)
-- - `trimmed`: extra comments and separators of the code.
-- - `obj`: the original object the code comes from
type RenderingData = {
    left : String,
    right : String,
    trimmed : String,
    obj: Object
}

-- Concatenates the left and right parts of the code without showing a preview.
-- The hider is applied to the full right side.
let getCodeWithoutPreview : CodeHider -> RenderingData -> String = lam hider. lam data.
    hider (concat data.left data.right)

-- Applies preview logic to the code rendering.
-- - If `right` is empty, then all content is shown directly.
-- - Otherwise, only `left` is visible and `right` is hidden via the hider.
-- - `trimmed` is always shown, as a minimal representation (e.g., `...` or type info)    
let getCodeWithPreview : CodeHider -> RenderingData -> String = lam hider. lam data.
    match data.right with [] then
        concatAll [data.left, data.trimmed]
    else 
        concatAll [data.left, hider data.right, data.trimmed]

-- A formatted code node, created from a `RenderingData` record.
-- Typically used for child objects that were already rendered.    
con TreeSourceCodeNode : RenderingData -> TreeSourceCode -- Formated code

-- A raw snippet of `SourceCodeWord`s that has not yet been rendered.
-- Typically represents a sequence of tokens like `let x =`.
con TreeSourceCodeSnippet : [SourceCodeWord] -> TreeSourceCode -- Array of not formated word
