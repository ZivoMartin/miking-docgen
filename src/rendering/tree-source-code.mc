    
type TreeSourceCode


type SonRenderingData = {
    left : [TreeSourceCode],
    right : [TreeSourceCode],
    trimmed : [TreeSourceCode],
    obj: ObjectTree
}

let getCodeWithoutPreview : SonRenderingData -> String = error "todo"
let getCodeWithPreview : SonRenderingData -> String = error "todo"    
    
con TreeSourceCodeNode : SonRenderingData -> TreeSourceCode -- Formated code
con TreeSourceCodeSnippet : [SourceCodeWord] -> TreeSourceCode -- Array of not formated word
