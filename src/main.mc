include "compile.mc"
    
mexpr
    use TokenReader in
    switch parse "src/token-readers.mc"
    case Some result then
        displayTree result
        --compileToMd result []
    end

