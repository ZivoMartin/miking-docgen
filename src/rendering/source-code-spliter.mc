-- This module defines a primitive used by renderer to split the code source, enabling them to know where they have to inject the button
-- For instance, with the code `let x = 3`, the module will split on the equal symbol.
-- For some nodes, we do not have to split. For exemple with use nodes that are not heavy 

include "../parsing/token-readers.mc"
include "./rendering-types.mc"

type Trimmed
con TrimmedFormated : String -> Trimmed
con TrimmedNotFormated : [SourceCodeWord] -> Trimmed

type SourceCodeSplit = { left: [TreeSourceCode], right: [TreeSourceCode], trimmed: Trimmed }
        
let sourceCodeSplit : [TreeSourceCode] -> SourceCodeSplit = use TokenReader in lam arr.
    let finish = lam left. lam right.
        switch right
        case [TreeSourceCodeSnippet arr] ++ right then
            let arr = reverse arr in
            match splitOnR (lam w. match w with { word = WeakComment {} | Comment {} | Separator {} } then false else true) arr with
                { left = trimmedRight, right = trimmedLeft } in
            let trimmedLeft = TreeSourceCodeSnippet (reverse trimmedLeft) in
            let trimmedRight = TrimmedNotFormated (reverse trimmedRight) in
            { left = left, right = cons trimmedLeft right, trimmed = trimmedRight }
        case [TreeSourceCodeNode { left = lastLeft, right = lastRight, trimmed = lastTrimmed, obj = obj }] ++ rightRev then
            let right = reverse (cons (TreeSourceCodeNode { left = lastLeft, right = lastRight, trimmed = [], obj = obj }) rightRev) in
            { left = left, right = right, trimmed = TrimmedFormated lastTrimmed }
        end 
    in

    let mergeAndFinish = lam left. lam right1. lam right2.
        finish [TreeSourceCodeSnippet left] (cons (TreeSourceCodeSnippet right1) right2) in

    match arr with [TreeSourceCodeSnippet buffer] ++ right then
        match buffer with [{ word = Word { content = first } } & x1] ++ rest then
    
        let splitAndReturn = lam split: String.
            match splitOnL (lam w. match w with { word = word } in eqString (lit word) split) rest with
                { left = left, right = rest } in
            mergeAndFinish (cons x1 left) rest right in
        
        switch first
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
        else finish [] arr
    else finish [] arr
