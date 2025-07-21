-- # Source Code Splitter
--
-- This module defines a utility for **splitting Miking source code into logical segments**, used by the rendering pipeline.
--
-- The main goal is to identify where a button or interaction should be injected — for example, at the `=` in `let x = 3`.
-- 
-- It works by analyzing the top-level structure of parsed source code and determining:
-- - What code comes before a relevant delimiter (like `=` or `:`)
-- - What code comes after
-- - What part should be `trimmed` and passed along for formatting or omission

include "../parsing/token-readers.mc"
include "./rendering-types.mc"


-- ## Trimmed
--
-- Represents what remains in the `right` side of the split, either raw or already formatted.
type Trimmed
con TrimmedFormated : String -> Trimmed
con TrimmedNotFormated : [SourceCodeWord] -> Trimmed

-- ## SourceCodeSplit
--
-- The result of a split operation.
-- - `left`: Code before the split (e.g., `let x`)
-- - `right`: Code after the split (e.g., `3`)
-- - `trimmed`: The rest of the right side, composed of comments and separators
type SourceCodeSplit = { left: [TreeSourceCode], right: [TreeSourceCode], trimmed: Trimmed }


-- ## sourceCodeSplit
--
-- Splits a TreeSourceCode array into two parts, based on semantic keywords and delimiters.
-- - Recognizes patterns like `let x = ...`, `con T : ...`, etc.
-- - Special-cases `use`, `lang`, `utest`, etc.
--
-- ### Input:
-- A list of `TreeSourceCode` blocks
--
-- ### Output:
-- A `SourceCodeSplit` structure with split metadata        
let sourceCodeSplit : [TreeSourceCode] -> SourceCodeSplit = use TokenReader in lam arr.
    let finish = lam left. lam right.
        let rightRev = reverse right in
        switch rightRev
        case [TreeSourceCodeSnippet arr] ++ rightRev then
            let arr = reverse arr in
            match splitOnR (lam w. match w with { word = WeakComment {} | Comment {} | Separator {} } then false else true) arr with
                { left = trimmedRight, right = trimmedLeft } in
            let trimmedLeft = TreeSourceCodeSnippet (reverse trimmedLeft) in
            let trimmedRight = TrimmedNotFormated (reverse trimmedRight) in
            { left = left, right = reverse (cons trimmedLeft rightRev), trimmed = trimmedRight }
        case [TreeSourceCodeNode data] ++ rightRev then
            let right = reverse (cons (TreeSourceCodeNode { data with trimmed = [] }) rightRev) in
            { left = left, right = right, trimmed = TrimmedFormated data.trimmed }
        end 
    in

    let mergeAndFinish = lam left. lam right1. lam right2.
        finish [TreeSourceCodeSnippet left] (cons (TreeSourceCodeSnippet right1) right2) in
    match arr with [TreeSourceCodeSnippet buffer] ++ right then
        match buffer with [{ word = Word {} } & x1] ++ rest then
    
        let splitAndReturn = lam split: String.
            match splitOnL (lam w. match w with { word = word } in eqString (lit word) split) rest with
                { left = left, right = rest } in
            mergeAndFinish (cons x1 left) rest right in
        switch content x1.word
        case "let" | "type" | "sem" | "syn" then splitAndReturn "="
        case "con" then splitAndReturn ":"
        case "use" then finish arr right
        case "utest" | "mexpr" then mergeAndFinish [x1] rest right
        case "lang" then
            match splitOnL (lam w. match w with { word = Word {} } then true else false) rest with
                { left = left, right = rest } in
            mergeAndFinish (cons x1 left) rest right
        case _ then finish [] arr
        end
        else
            finish [] arr
    else finish [] arr
