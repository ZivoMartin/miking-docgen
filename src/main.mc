include "compiler/compile.mc"
    
mexpr
    use TokenReader in
    switch parse "src/main.mc"
    case Some result then
        -- displayTree result
        compileToMd result []
    end

