-- This module defines a primitive used by renderer to split the code source, enabling them to know where they have to inject the button
-- For instance, with the code `let x = 3`, the module will split on the equal symbol.
-- For some nodes, we do not have to split. For exemple with use nodes that are not heavy 

include "../extracting/source-code-reconstruction.mc"
include "../parsing/token-readers.mc"


-- "type" | "con" | "lang" | "syn" | "use" | "mexpr" | "utest" | "with" | "recursive" | "match" | "end" | "switch" | "in" | "case" | "if" | "else" | "let" | "lam" | "sem"
-- "con" | "lang" | "syn" | "sem"    
    
let sourceCodeSplit : [TreeSourceCode] -> { left: [TreeSourceCode], right: [TreeSourceCode] } = use TokenReader in lam arr.
    let finish = lam left. lam right1. lam right2.
        { left = [TreeSourceCodeSnippet left], right = cons (TreeSourceCodeSnippet right1) right2 } in
    match arr with [TreeSourceCodeSnippet buffer] ++ right then
        match buffer with [{ word = Word { content = first } } & x1] ++ rest then
    
        let splitAndReturn = lam split.
            match splitOn (lam w. match w with { word = Word { content = split } } then true else false) rest with
                { left = left, right = rest } in
            finish (cons x1 left) rest right in
        
        switch first
        case "let" | "type" | "sem" | "syn" then splitAndReturn "="
        case "con" then splitAndReturn ":"
        case "use" then { left = arr, right = []}
        case "utest" | "mexpr" then finish [x1] rest right
        case "lang" then
            match splitOn (lam w. match w with { word = Word {} } then true else false) rest with
                { left = left, right = rest } in
            finish (cons x1 left) rest right
            
        case _ then { left = [], right = arr }
        end
        else
            warn "Detected an empty SourceCodeSnippet, should never happend.";
            { left = [], right = arr }
    else 
        { left = [], right = arr }
