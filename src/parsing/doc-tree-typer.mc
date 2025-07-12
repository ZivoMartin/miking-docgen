include "./types-stream.mc"
include "./doc-tree.mc"

    
let typeDocTree : DocTree -> String -> DocTree =
    use TokenReader in use BreakerChooser in use TypeStream in lam tree. lam filePath.
    recursive let getNextName: [DocTree] -> String = lam sons.
        switch sons 
        case [Leaf { token = Word { content = "#var" } }, Leaf { token = Str { between = name } } ] ++ _ then
            name
        case [Leaf { token = Word { content = name } }] ++ _ then
            name        
        case [_] ++ sons then getNextName sons
        end in
    
    recursive let typeDocTree : TypeStreamContext -> Option String -> DocTree -> { ctx : TypeStreamContext, tree : DocTree } = lam ctx. lam langName. lam tree.

        type FoldRes = { ctx : TypeStreamContext, sons: [DocTree] } in
        let foldSons: [DocTree] -> Option String -> TypeStreamContext -> FoldRes = lam sons. lam langName. lam ctx.
                let fRes = foldl (
                    lam a: FoldRes. lam s: DocTree.
                    match typeDocTree a.ctx langName s with { ctx = ctx, tree = tree} in
                    { ctx = ctx, sons = cons tree a.sons }
                ) { ctx = ctx, sons = [] } sons in
                { fRes with sons = reverse fRes.sons } in
        let default = { tree = tree, ctx = ctx } in
        switch tree
        case Node { state = Let {} | TopLet {} | Rec {} | TopRec {} | Sem {}} & Node d then
            let name =  concat (match langName with Some name then concatAll ["v", name, "_"] else "") (getNextName d.sons) in
            let t = typeStreamNext name ctx in
            match foldSons d.sons (None {}) t.ctx with { ctx = ctx, sons = sons} in
            (match t.t with None {} then parsingWarn (concatAll ["Found a typeless token: ", int2string d.pos.x, " ", int2string d.pos.y]) else ());
            { tree = Node { d with sons = sons, ty = t.t}, ctx = ctx }
        case Node { state = Utest {} | TopUtest {} } then default
        case Node { state = Lang {} } & Node d then
            match foldSons d.sons (Some (getNextName d.sons)) ctx  with { ctx = ctx, sons = sons} in
            { tree = Node { d with sons = sons }, ctx = ctx }
        case Node d then
            match foldSons d.sons (None {}) ctx  with { ctx = ctx, sons = sons} in
            { tree = Node { d with sons = sons }, ctx = ctx }
        case IncludeNode { tree = Some t, path = path } & IncludeNode d then
            parsingLog (concat "Entering in" path);
            match typeDocTree ctx (None {}) t with { ctx = ctx, tree = tree } in
                parsingLog (concat path " is labeled");
                { tree = IncludeNode { d with tree = Some tree }, ctx = ctx }
        case tree then default
        end
    in
    parsingLog "Computing ast...";
    let ctx = buildTypeStream filePath in
    parsingLog "Labeling types..";
    (typeDocTree ctx (None {}) tree).tree 
