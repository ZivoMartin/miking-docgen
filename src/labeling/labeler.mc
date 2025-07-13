include "./types-stream.mc"
include "../extracting/objects.mc"
include "../util.mc"

let label : ObjectTree -> ObjectTree =
    use ObjectKinds in use TypeStream in use RemoveMetaVar in lam tree.

    type SkipedContext = { ctx: TypeStreamContext, t: Type } in
    type LangContext = { langName: String, semMap: HashMap String SkipedContext } in
    type LabelRecRes = { ctx : TypeStreamContext, tree : ObjectTree, langContext : Option LangContext } in

    recursive let labelRec : TypeStreamContext -> String -> Option LangContext -> ObjectTree -> LabelRecRes =
        lam ctx. lam fileName. lam langContext. lam tree.
    
        type FoldRes = { ctx : TypeStreamContext, sons: [ObjectTree], langContext : Option LangContext } in
        let foldSons: [ObjectTree] -> Option LangContext -> TypeStreamContext -> FoldRes =
            lam sons. lam langContext. lam ctx.
                let fRes = foldl (
                    lam a: FoldRes. lam s: ObjectTree.
                    match labelRec a.ctx fileName a.langContext s with { ctx = ctx, tree = tree, langContext = langContext } in
                    { ctx = ctx, sons = cons tree a.sons, langContext = langContext }
                ) { ctx = ctx, langContext = langContext, sons = [] } sons in
                { fRes with sons = reverse fRes.sons } in
    
        let default = { tree = tree, ctx = ctx, langContext = langContext } in
        let buildRes = lam obj. lam ctx. lam langContext. lam sons. lam ty.
            let ty = match ty with Some t then Some (removeMetaVarType t) else None {} in
            {
                    tree = ObjectNode {
                            sons = sons,
                            obj = objSetType obj ty
                            },
                    ctx = ctx,
                    langContext = langContext
                }
        in
    
        match tree with ObjectNode { obj = { kind = kind, name = name, namespace = namespace } & obj, sons = sons } then
            let warn = lam. labelingWarn (join ["Found a typeless token : ", fileName]) in
            switch kind
            case ObjLet {} then
                match typeStreamNext name ctx with { t = t, ctx = ctx } in
                (match t with None {} then warn () else ());
                match foldSons sons (None {}) ctx with { ctx = ctx, sons = sons} in
               buildRes obj ctx langContext sons t
            case ObjSem {} then
                match langContext with Some { langName = langName, semMap = semMap } then
                    let prefix = join ["v", langName, "_"] in
                    let name = concat prefix name in
                    match hmLookup name semMap with Some { t = t, ctx = tmpCtx } then
                        match foldSons sons (None {}) tmpCtx with { ctx = tmpCtx, sons = sons} in
                        let semMap = hmInsert name { ctx = tmpCtx, t = t } semMap in
                        buildRes obj ctx (Some { langName = langName, semMap = semMap }) sons (Some t)        
                    else
                        let updateLangContext = lam semMap. lam skiped.
                            let semMap =  foldl (lam semMap. lam skip.
                                hmInsert skip.name { t = skip.t, ctx = typeStreamFromExpr skip.body } semMap)
                                semMap (filter (lam s. strStartsWith prefix s.name) skiped) in
                            Some { semMap = semMap, langName = langName }
                        in

                        switch typeStreamNext name ctx    
                        case { t = Some t, ctx = ctx, skiped = skiped } then
                            match typeStreamPop ctx with { ctx = ctx, ast = ast } in
                            let tmpCtx = typeStreamFromExpr ast in
                            match foldSons sons (None {}) tmpCtx with { ctx = tmpCtx, sons = sons} in
                            let semMap = hmInsert name { t = t, ctx = tmpCtx } semMap in
                            buildRes obj ctx (updateLangContext semMap skiped) sons (Some t)    
                        case { t = None {}, ctx = ctx, skiped = skiped } then
                            warn ();
                            buildRes obj ctx (updateLangContext semMap skiped) sons (None {})
                        end                    
                else
                    labelingWarn "Lang context in None, should never happend while labeling a Sem"; default
            case ObjUtest {} | ObjMexpr {} then default
            case ObjLang {} then
                let langContext = Some { langName = name, semMap = hashmapEmpty () } in
                match foldSons sons langContext ctx  with { ctx = ctx, sons = sons } in
                buildRes obj ctx (None {}) sons (None {})
            case ObjInclude { pathInFile = pathInFile } then
                switch sons
                case [program] then    
                    labelingLog (concat "Labeling in" namespace);
                    match labelRec ctx pathInFile (None {}) program with { ctx = ctx, tree = tree } in
                    labelingLog (concat namespace " is labeled");
                    buildRes obj ctx langContext [tree] (None {})
                case [] then default
                case _ then labelingWarn "Include objects should all have 0 or 1 son."; default
                end
            case _ then
                match foldSons sons (None {}) ctx with { ctx = ctx, sons = sons} in
                buildRes obj ctx langContext sons (None {})            
            end
        else default
    in
    match tree with ObjectNode { obj = { namespace = filePath } } then
        labelingLog "Computing ast...";
        let ctx = buildTypeStream filePath in
        labelingLog "Labeling types..";
        (labelRec ctx filePath (None {}) tree).tree 
    else
        labelingWarn "Labeling failed, top object should always be a Node.";
        tree
