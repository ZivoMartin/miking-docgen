-- This module defines a primitive used by renderer to split the code source, enabling them to know where they have to inject the button
-- For instance, with the code `let x = 3`, the module will split on the equal symbol.
-- For some nodes, we do not have to split. For exemple with use nodes that are not heavy 

include "../extracting/source-code-reconstruction.mc"
include "../parsing/token-readers.mc"

let sourceCodeSplit : [TreeSourceCode] -> { left: [TreeSourceCode], right: [TreeSourceCode] } = use TokenReader in lam arr.
    match arr with [TreeSourceCodeSnippet buffer] ++ right then
        switch buffer
        case [{ word = Word { content = "let" } } & x1] ++ rest then
            match splitOn (lam w. match w with { word = Word { content = "=" } } then true else false) rest with
                { left = left, right = rest } in
            { left = [TreeSourceCodeSnippet (cons x1 left)], right = cons (TreeSourceCodeSnippet rest) right}
        case _ then { left = [], right = arr }
        end
    else 
        { left = [], right = arr }
