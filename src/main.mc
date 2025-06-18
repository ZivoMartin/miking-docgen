include "compile.mc"
    
mexpr
    use TokenReader in
    switch parse "src/doc-tree.mc"
    case Some result then
        --displayTree result
        compileToMd result []
    end

