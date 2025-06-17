include "compile.mc"
    
mexpr
    use TokenReader in
    let result = parse "src/token-readers.mc" in
    compileToMd result []
    --displayTree result
