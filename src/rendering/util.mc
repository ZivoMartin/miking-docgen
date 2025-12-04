-- # util.mc
--
-- This module defines primitives used by the renderer to handle objects
-- during the documentation generation process.

include "./rendering-types.mc"
include "../extracting/objects.mc"
include "../global/util.mc"

-- ## removeDoubleNames
--
-- During rendering, we generate one page and one documentation block per child.
-- But what if two children have the same name and the same kind?
-- Since they share the same name, namespace and kind, they will end up with the same URL.
-- However, two documentation blocks will still be generated, both pointing
-- toward the last child’s page.
--
-- This function catches all duplicate names among children with same namespace
-- and keeps only the last one. Moreover, if two children
-- with the same namespace are next to each other, we merge their documentation.
-- Otherwise, in this scenario:
--
-- -- Takes x and returns x + 1
-- sem semX: Int -> Int
-- sem semX = | x -> addi x 1
--
-- Since only the last sem remains, the previous documentation would be lost.
let removeDoubleNames : [RenderingData] -> [RenderingData] = lam children.
    use ObjectsRenderer in

    type MergeFoldArg = { doc: String, prev: String, children: [RenderingData] } in
    -- Merging the documentations of consecutive same elements.
    let merged = foldl
    (
        lam arg. lam child.
        match arg with { doc = doc, prev = prev, children = children } in
        let obj = child.obj in
        let namespace = objNamespace obj in
        if not (objHasName obj) then
           { arg with doc = "", children = cons child children, prev = "" }
        else if eqString namespace prev then
           let doc = if eqString objDefaultDoc doc then "" else doc in
           let newDoc = objTryGetDoc child.obj in
           let doc = concat doc newDoc in
           let child = { child with obj = objWithDoc child.obj doc } in
           { arg with doc = doc, children = cons child children }
        else
           { arg with doc = objDoc child.obj, children = cons child children, prev = namespace }
        
    ) { doc = "", prev = "", children = [] } children in

    type SanitizeFoldArg = { saw: HashMap String (), children: [RenderingData] } in
    -- Removing double names
    let sanitized =  foldl
    (
        lam arg. lam child.
        match arg with { saw = saw, children = children } in
        let namespace = objNamespace child.obj in
        
        if objHasName child.obj then
           match hmLookup namespace saw with Some _ then printLn namespace;arg
           else { children = cons child children, saw = hmInsert namespace () saw }
        else { arg with children = cons child children }
    ) { children = [], saw = hashmapEmpty () } merged.children in
    sanitized.children
        

-- ## RenderingDataSet
--
-- Groups `RenderingData` nodes into categories by their kind.
-- This structure is useful for organizing sections in the documentation.
type RenderingDataSet = {
    sUse: [Object],
    sLet: [RenderingData],
    sLang: [RenderingData],
    sSem: [RenderingData],
    sSyn: [RenderingData],
    sCon: [RenderingData],
    sMexpr: [RenderingData],
    sInclude: [Object],
    sLibInclude: [Object],
    sType: [RenderingData],
    sUtest: [RenderingData]
}
    
-- Constructs a `RenderingDataSet` from:
-- - A list of rendered children (`children`).
-- - Recursive block data (`recDatas`), extracted earlier.
let buildSet: [RenderingData] -> [[RenderingData]] -> RenderingDataSet =
    use ObjectKinds in
    lam children. lam recDatas.
    recursive
    let buildSet = lam set. lam children. lam recDatas.
        switch children
        case [child] ++ children then
            let switchRes = switch child.obj.kind
            case ObjUse {} then ({ set with sUse = cons child.obj set.sUse }, recDatas)
            case ObjLet {} then ({ set with sLet = cons child set.sLet }, recDatas)
            case ObjLang {} then ({ set with sLang = cons child set.sLang }, recDatas)
            case ObjSem {} then ({ set with sSem = cons child set.sSem }, recDatas)
            case ObjSyn {} then ({ set with sSyn = cons child set.sSyn }, recDatas)
            case ObjCon {} then ({ set with sCon = cons child set.sCon }, recDatas)
            case ObjMexpr {} then ({ set with sMexpr = cons child set.sMexpr }, recDatas)
            case ObjType {} then ({ set with sType = cons child set.sType }, recDatas)
            case ObjUtest {} then ({ set with sUtest = cons child set.sUtest }, recDatas)
            case ObjInclude {} then
                let set = if objIsStdlib child.obj then
                    { set with sLibInclude = cons child.obj set.sLibInclude }
                  else
                    { set with sInclude = cons child.obj set.sInclude }
                in
                (set, recDatas)
            case ObjRecursiveBloc {} then
                match recDatas with [children] ++ recDatas then
                    ({ set with sLet = concat children set.sLet }, recDatas)
                else
                   renderingWarn "Running out of recursive datas.";
                   (set, recDatas)
            end in
            match switchRes with (set, recDatas) in
            buildSet set children recDatas
        case [] then set
        end
    in buildSet { sUse = [], sLet = [], sLang = [],  sSem = [], sSyn = [], sCon = [], sMexpr = [], sInclude = [], sLibInclude = [], sType = [], sUtest = [] } (reverse children) (reverse recDatas)


let unwrapRecursives : RenderingOptions -> [ObjectTree] -> [{ children: [ObjectTree], tests: [ObjectTree] }] =
    use ObjectKinds in
    lam opt. lam children.
    let res = foldl (lam buffer. lam tree.
        let obj = objTreeObj tree in
        switch obj.kind
        case ObjRecursiveBloc {} then
            let children = objTreeChildren tree in
            match children with [first] ++ rest then
                let firstObj = objTreeObj first in
                let firstDoc = objTryGetDoc firstObj in
                let firstObj = if null firstDoc then objWithDoc firstObj (objDoc obj) else firstObj in
                let first = objTreeWithObj first firstObj in
                let children = cons first rest in
                let result = cons { children = children, tests = buffer.testBuffer } buffer.result in
                { result = result, testBuffer = [] }
            else buffer
        case ObjUtest {} then { buffer with testBuffer = cons tree buffer.testBuffer }
        case _  then { buffer with testBuffer = [] }
        end) { result = [], testBuffer = [] } (reverse children)
    in
    res.result

let renderFileOrWarn : String -> String -> () = lam path. lam content.
    match fileWriteOpen path with Some wc then
          fileWriteString wc content;
          fileWriteClose wc
    else
          renderingWarn (concat "Failed to create search file: " path)
