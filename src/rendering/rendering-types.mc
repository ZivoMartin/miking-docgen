type WordRenderer = SourceCodeWord -> String
type CodeHider = String -> String
    
type TreeSourceCode

type RenderingData = {
    left : String,
    right : String,
    trimmed : String,
    obj: Object
}

let getCodeWithoutPreview : CodeHider -> RenderingData -> String = lam hider. lam data.
    hider (concat data.left data.right)
    
let getCodeWithPreview : CodeHider -> RenderingData -> String = lam hider. lam data.
    match data.right with [] then
        concatAll [data.left, data.trimmed]
    else 
        concatAll [data.left, hider data.right, data.trimmed]

con TreeSourceCodeNode : RenderingData -> TreeSourceCode -- Formated code
con TreeSourceCodeSnippet : [SourceCodeWord] -> TreeSourceCode -- Array of not formated word
