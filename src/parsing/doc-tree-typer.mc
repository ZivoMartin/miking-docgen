include "./types-stream.mc"
include "./doc-tree.mc"
include "../util.mc"
    
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

    type LangContext = { langName: String, semMap: HashMap String Type } in
        
    recursive let typeDocTree : TypeStreamContext -> String -> Option LangContext -> DocTree -> { ctx : TypeStreamContext, tree : DocTree, langContext : Option LangContext } = lam ctx. lam fileName. lam langContext. lam tree.

        type FoldRes = { ctx : TypeStreamContext, sons: [DocTree], langContext : Option LangContext } in
        let foldSons: [DocTree] -> Option LangContext -> TypeStreamContext -> FoldRes = lam sons. lam langContext. lam ctx.
                let fRes = foldl (
                    lam a: FoldRes. lam s: DocTree.
                    match typeDocTree a.ctx fileName a.langContext s with { ctx = ctx, tree = tree, langContext = langContext } in
                    { ctx = ctx, sons = cons tree a.sons, langContext = langContext }
                ) { ctx = ctx, langContext = langContext, sons = [] } sons in
                { fRes with sons = reverse fRes.sons } in
        let default = { tree = tree, ctx = ctx, langContext = langContext } in
        switch tree
        case Node { state = Let {} | TopLet {} | Rec {} | TopRec {} | Sem {}} & Node d then
            let warn = lam. parsingWarn (concatAll ["Found a typeless token: ", fileName,  ", ", int2string d.pos.x, " ", int2string d.pos.y]) in
            match (match langContext with Some { langName = langName, semMap = semMap } then
                    let prefix = concatAll ["v", langName, "_"] in
                    let name = concat prefix (getNextName d.sons) in
                    match hmLookup name semMap with Some t then
                        { t = Some t, langContext = langContext, ctx = ctx }
                    else
                        let addSkipedSems = lam semMap. lam skiped.
                            foldl (lam semMap. lam skip. hmInsert skip.name skip.t semMap) semMap (filter (lam s. strStartsWith prefix s.name) skiped)
                         in
                        switch typeStreamNext name ctx
                        case { t = Some t, ctx = ctx, skiped = skiped } then
                            let semMap = hmInsert name t semMap in
                            { t = Some t, ctx = ctx, langContext = Some { semMap = addSkipedSems semMap skiped, langName = langName } }
                        case { t = None {}, ctx = ctx, skiped = skiped } then
                            warn ();
                            { t = None {}, ctx = ctx, langContext = Some { semMap = addSkipedSems semMap skiped, langName = langName } }
                        end
                else
                    match typeStreamNext (getNextName d.sons) ctx with { t = t, ctx = ctx } in
                    (match t with None {} then warn () else ());
                    { t = t, ctx = ctx, langContext = langContext })
            with { langContext = langContext, t = t, ctx = ctx } in

            match foldSons d.sons (None {}) ctx with { ctx = ctx, sons = sons} in
            { tree = Node { d with sons = sons, ty = t}, ctx = ctx, langContext = langContext }
        case Node { state = Utest {} | TopUtest {} } then default
        case Node { state = Lang {} } & Node d then
            let langContext = Some { langName = getNextName d.sons, semMap = hashmapEmpty () } in
            match foldSons d.sons langContext ctx  with { ctx = ctx, sons = sons } in
            { tree = Node { d with sons = sons }, ctx = ctx, langContext = None {} }
        case Node d then
            match foldSons d.sons (None {}) ctx  with { ctx = ctx, sons = sons} in
            { tree = Node { d with sons = sons }, ctx = ctx, langContext = langContext }
        case IncludeNode { tree = Some t, path = path } & IncludeNode d then
            parsingLog (concat "Labeling in" path);
            match typeDocTree ctx path (None {}) t with { ctx = ctx, tree = tree } in
            parsingLog (concat path " is labeled");
            { tree = IncludeNode { d with tree = Some tree }, ctx = ctx, langContext = langContext }

        case tree then default
        end
    in
    parsingLog "Computing ast...";
    let ctx = buildTypeStream filePath in
    parsingLog "Labeling types..";
    (typeDocTree ctx filePath (None {}) tree).tree 
