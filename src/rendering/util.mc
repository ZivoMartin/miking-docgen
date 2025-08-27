-- This file defines primitive used by the renderer to handle the objects.

include "./rendering-types.mc"

-- This function goes through all the nodes and extract the recursive children.
-- It avoids having a useless recursive documentation bloc.
let unwrapRecursives : [ObjectTree] -> [ObjectTree] = use ObjectKinds in lam sons.
    foldl (lam sons. lam son.
        switch son
        case ObjectNode { obj = { kind = ObjRecursiveBlock {}, doc = doc, sourceCode = sourceCode }, sons = blockSons } then
            let blockSons = foldl (lam acc. lam son.
                let res = cons son acc in
                switch son
                case ObjectNode { obj = { kind = ObjLet {}, name = name } } then res
                case ObjectNode { obj = { kind = kind, name = name } } then
                     
                     renderingWarn (join ["We should only have let node at this stage. Found ", objKindToString kind, " ", name]);
                     res
                end) [] blockSons in
            match sourceCodeTrim sourceCode with { left = left, right = right } in
            let blockSons = reverse blockSons in
            let blockSons =
                match blockSons with [h] ++ t then
                    let newDoc = concat (objTreeDoc son) (objTreeDoc h) in
                    let newSourceCode = concat left (objTreeSourceCode h) in
                    cons (objTreeWithSourceCode (objTreeWithDoc h newDoc) newSourceCode) t
                else blockSons
            in
            concat blockSons sons
        case ObjectNode {} then cons son sons
        case _ then sons
        end) [] (reverse sons) 

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

type RenderingDataSet = { Use: [Object], Let: [RenderingData], Lang: [RenderingData],  Sem: [RenderingData], Syn: [RenderingData], Con: [RenderingData], Mexpr: [RenderingData], Include: [Object], LibInclude: [Object], Type: [RenderingData], Utest: [RenderingData] }

let buildSet: [RenderingData] -> RenderingDataSet = use ObjectKinds in lam sons. 
    recursive
    let buildSet = lam set. lam sons.
        switch sons
        case [son] ++ sons then buildSet (
        switch son.obj.kind
            case ObjUse {} then { set with Use = cons son.obj set.Use }
            case ObjLet {} then { set with Let = cons son set.Let }
            case ObjLang {} then { set with Lang = cons son set.Lang }
            case ObjSem {} then { set with Sem = cons son set.Sem }
            case ObjSyn {} then { set with Syn = cons son set.Syn }
            case ObjCon {} then { set with Con = cons son set.Con }    
            case ObjMexpr {} then { set with Mexpr = cons son set.Mexpr }
            case ObjType {} then { set with Type = cons son set.Type }
            case ObjUtest {} then { set with Utest = cons son set.Utest }
            case ObjInclude {} then
                if objIsStdlib son.obj then { set with LibInclude = cons son.obj set.LibInclude } else { set with Include = cons son.obj set.Include }
            case ObjRecursiveBlock {} then renderingWarn "We should not get to RecursiveBlock at this stage."; set
            end) sons
        case [] then set
        end
    in buildSet { Use = [], Let = [], Lang = [],  Sem = [], Syn = [], Con = [], Mexpr = [], Include = [], LibInclude = [], Type = [], Utest = [] } (reverse sons)
