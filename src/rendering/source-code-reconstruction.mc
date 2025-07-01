include "./renderer-interface.mc"
include "./source-code-spliter.mc"
include "../extracting/source-code-builder.mc"
include "../extracting/source-code-word.mc"    
include "./tree-source-code.mc"

    
let getRenderingData : ObjectTree -> SourceCode -> [SonRenderingData] -> SonRenderingData = lam obj. lam code. lam sons.
    type Arg = {
        tree: [TreeSourceCode],
        sons: [SonRenderingData],
        buffer: [SourceCodeWord]
    } in
    let tree = foldl (lam a: Arg. lam word: Option SourceCodeWord.
        switch word
        case Some w then { a with buffer = cons w a.buffer}
        case None {} then
            match sons with [son] ++ sons then
                match a.buffer with [] then
                    { a with tree = cons (TreeSourceCodeNode son) a.tree, sons = sons }
                else
                    { tree = concat [TreeSourceCodeNode son, TreeSourceCodeSnippet a.buffer] a.tree, sons = sons, buffer = [] }
            else
                warn "Son array should not be empty at this point";
                a
        end) { tree = [], sons = sons, buffer = [] } code in
    let tree = reverse (match tree.buffer with [] then tree.tree else cons (TreeSourceCodeSnippet tree.buffer) tree.tree) in
    match sourceCodeSplit tree with { left = left, right = right, trimmed = trimmed } in
    error "todo"    
