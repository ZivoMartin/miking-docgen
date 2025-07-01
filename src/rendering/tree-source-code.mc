
type WordRenderer = SourceCodeWord -> String
type CodeHider = String -> String
    
type TreeSourceCode


type SonRenderingData = {
    left : String,
    right : String,
    trimmed : String,
    obj: ObjectTree
}

let getCodeWithoutPreview : CodeHider -> SonRenderingData -> String = lam hider. lam data.
    hider (concat data.left data.right)
    
let getCodeWithPreview : CodeHider -> SonRenderingData -> String = lam hider. lam data.
    concatAll [data.left, hider data.right, data.trimmed]

con TreeSourceCodeNode : SonRenderingData -> TreeSourceCode -- Formated code
con TreeSourceCodeSnippet : [SourceCodeWord] -> TreeSourceCode -- Array of not formated word
