-- # util.mc
--
-- This module defines primitives used by the renderer to handle objects
-- during the documentation generation process.

include "./rendering-types.mc"

-- ## injectTests
--
-- Post-processes a list of `RenderingData` nodes to attach unit tests to their parent
-- documentation blocks.
-- - Iterates through children (`sons`).
-- - Buffers any `RenderingData` elements that correspond to test code.
-- - When a new non-test block (`current`) is encountered, merges the buffered tests
--   into that block by:
--   * Concatenating the test code (`tests`) into the `tests` field
--   * Concatenating their raw rows (`row`) into the `rowTests` field
-- - Clears the buffer after attaching tests.
--
-- Returns: the transformed list of `RenderingData`, where test blocks are folded into
-- the corresponding parent documentation block.
let injectTests : [RenderingData] -> [RenderingData] = use ObjectKinds in lam sons.
    type Arg = { current: Option RenderingData, acc: [RenderingData], tests: [RenderingData] } in

    -- - Takes the accumulated list, the current block, and buffered tests.
    -- - Extracts the "lastRow" of the last test, trimming trailing comments and empty lines.
    -- - Builds the final `tests` and `rowTests` strings by concatenating buffered test code.
    -- - Produces a new `RenderingData` with its tests attached, then pushes it into the accumulator.
    let pushSonInAcc: [RenderingData] -> RenderingData -> [RenderingData] -> [RenderingData] = lam acc. lam current. lam tests.
        let testsStr: (String, String) =
            match tests with [last] ++ tests then
                let lastRow = last.row in
                recursive let trimRow = lam row.
                  match row with [h] ++ t then
                        let l = strTrim h in
                        if strStartsWith "--" l then
                           trimRow t
                        else if eqString l "" then
                           trimRow t
                        else strJoin "\n" (reverse row)
                  else []
                in
                let lastRow = trimRow (reverse (strSplit "\n" lastRow)) in
                
                let row: String = join (map (lam t. t.row) (reverse tests)) in                            
                let tests: String = join (map (lam t. join [t.left, t.right, t.trimmed]) (reverse tests)) in
                (join [tests, last.left, last.right], concat row lastRow)
            else ("", "")
        in
        let current: RenderingData = { current with tests = testsStr.0, rowTests = testsStr.1 } in
        join [tests, [current], acc]
    in
                    
    let foldRes: Arg = foldl (lam arg. lam son.
        match arg with { current = current, acc = acc, tests = tests } in
        let isUtest = match objKind son.obj with ObjUtest {} then true else false in
        switch current
        case Some current then
             if isUtest then { arg with tests = cons son tests }
             else { arg with current = Some son, tests = [], acc = pushSonInAcc acc current tests }
        case None {} then
             if isUtest then { arg with acc = cons son acc }
             else { arg with current =  Some son }
        end
    ) { current = None {}, acc = [], tests = [] } sons
    in
    
    let sons = switch foldRes.current
        case Some current then pushSonInAcc foldRes.acc current foldRes.tests
        case None {} then foldRes.acc
        end
    in
    
    reverse sons

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
-- - A list of rendered children (`sons`).
-- - Recursive block data (`recDatas`), extracted earlier.
let buildSet: [RenderingData] -> [[RenderingData]] -> RenderingDataSet = use ObjectKinds in lam sons. lam recDatas.
    recursive
    let buildSet = lam set. lam sons. lam recDatas.
        switch sons
        case [son] ++ sons then
            let switchRes = switch son.obj.kind
            case ObjUse {} then ({ set with sUse = cons son.obj set.sUse }, recDatas)
            case ObjLet {} then ({ set with sLet = cons son set.sLet }, recDatas)
            case ObjLang {} then ({ set with sLang = cons son set.sLang }, recDatas)
            case ObjSem {} then ({ set with sSem = cons son set.sSem }, recDatas)
            case ObjSyn {} then ({ set with sSyn = cons son set.sSyn }, recDatas)
            case ObjCon {} then ({ set with sCon = cons son set.sCon }, recDatas)
            case ObjMexpr {} then ({ set with sMexpr = cons son set.sMexpr }, recDatas)
            case ObjType {} then ({ set with sType = cons son set.sType }, recDatas)
            case ObjUtest {} then ({ set with sUtest = cons son set.sUtest }, recDatas)
            case ObjInclude {} then
                (if objIsStdlib son.obj then { set with sLibInclude = cons son.obj set.sLibInclude } else { set with sInclude = cons son.obj set.sInclude }, recDatas)
            case ObjRecursiveBloc {} then
                match recDatas with [sons] ++ recDatas then
                    ({ set with sLet = concat sons set.sLet }, recDatas)
                else
                   renderingWarn "Running out of recursive datas.";
                   (set, recDatas)
            end in
            match switchRes with (set, recDatas) in
            buildSet set sons recDatas
        case [] then set
        end
    in buildSet { sUse = [], sLet = [], sLang = [],  sSem = [], sSyn = [], sCon = [], sMexpr = [], sInclude = [], sLibInclude = [], sType = [], sUtest = [] } (reverse sons) (reverse recDatas)
