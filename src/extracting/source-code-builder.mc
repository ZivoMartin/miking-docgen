include "../parsing/doc-tree.mc"

type CodeSourceWord = Option String

type CodeSourceBuilder
con CodeSourceNode : { parent: CodeSourceBuilder, buffer: [CodeSourceWord] } -> CodeSourceBuilder


let absorbWord : CodeSourceBuilder -> DocTree -> CodeSourceBuilder =
    use TokenReader in lam builder. lam word.
    match builder with CodeSourceNode { parent = parent, buffer = buffer } in    
    switch word
    case Node { token = token } then
        let parent = CodeSourceNode { parent = parent, buffer = cons (None {}) buffer} in
        CodeSourceNode { parent = parent, buffer = [lit token] }
    case Leaf { token = token } then CodeSourceNode { parent = parent, buffer = cons (Some (lit token)) buffer}
    end

let finish : CodeSourceBuilder -> { builder: CodeSourceBuilder, buffer: [CodeSourceWord] } = lam builder.
    match builder with CodeSourceNode { parent = parent, buffer = buffer } in
        { builder = parent, buffer = reverse buffer }
