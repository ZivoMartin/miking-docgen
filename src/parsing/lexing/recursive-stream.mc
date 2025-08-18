/-
We have here the implementation of the recursive-stream.
The idea is simple: we take as input an expression from the compiler representing a Miking program, which we use as a stack.
If we encounter a match, we push the 3 expressions onto the stack in the correct order so the traversal is performed properly.

The main goal is to provide the number of ins present in each recursive block. To do this, we travel down the stack until we reach the next TmRecLets. Once found, we traverse all its bindings (the lets contained inside), count the number of expected ins, and build a cache.
If other recursive blocks are nested inside the one we found, we also count their internal ins. We then obtain a cache array, which we store for the next call.

Of course, we need to be careful not to count the same in twice. For instance, in the following code:

recursive
    let x =
        recursive
            let y = let z = 2 in 3
        in
        4
end

The parent recursive block will generate a cache for the child recursive block. The child recursive counts only one in (the one closing z), and the parent recursive also counts one in (the one closing the child recursive), but it does not consider the let z.

The idea behind this method is that it lets us easily handle the case of languages with merged semantics. For example, consider the code:

lang A
sem x = | 1 -> ...
sem y = | _ -> ...
sem x = | 2 -> ...
end

Here, the compiler’s AST will only contain a single sem.
However, during lexing, we will see the sem y in between them. The solution is simple: whenever we encounter a lang, we locate it in the AST and create a cache array per semantic. We then store this in a hashmap linking the semantic names to their respective cache. When the lexer encounters the keyword sem, it gives us the found name, and we can then update the hashmap and switch to the appropriate cache.

The sem-stream is then used during the generation of the map, where we keep only the last n recursive blocks if n is the expected number of recursive blocks. The discarded recursive blocks are those that were added during assembly.
-/


include "mexpr/ast.mc"
include "../../global/util.mc"
include "./sem-map.mc"

type RecursiveDataStreamMap = HashMap String [Int]

type RecursiveDataStream = use MExprAst in {
     stack: [Expr],
     cache: [Int],
     langMap: RecursiveDataStreamMap,
     currentSem: String,
     langName: String
}

let removePrefix : String -> String = lam name. reverse (splitOnR (eqChar '_') (reverse name)).left 

let createRecursiveDataStream : use MExprAst in Expr -> RecursiveDataStream = use MExprAst in lam expr.
    { stack = [expr], cache = [], currentSem = "", langName = "", langMap = hashmapEmpty () }

type DataStreamComputeNextRes = { stream: RecursiveDataStream, acc: [Int], map: RecursiveDataStreamMap, inCount: Int }
let recursiveDataStreamComputeNext: RecursiveDataStream -> Option SemMap -> DataStreamComputeNextRes = use MExprAst in use MExprPrettyPrint in lam stream. lam semMap.
    type WorkRes = { acc: [Int], inCount: Int, stack: [Expr], map: RecursiveDataStreamMap } in  
    recursive let work : Bool -> [Int] -> [Expr] -> Int -> WorkRes = lam first. lam acc. lam stack. lam inCount.
        match stack with [] then { inCount = inCount, acc = acc, stack = [], map = hashmapEmpty () } else
        match stack with [expr] ++ rest in
        
        let go : [Expr] -> WorkRes = lam toAdd. work first acc (concat toAdd rest) inCount in
        let addAndGo : [Expr] -> WorkRes = lam toAdd. work first acc (concat toAdd rest) (addi inCount 1) in

        switch expr
        case TmRecLets { bindings = bindings, inexpr = inexpr } then
            type Arg = { recLetCount: Int, acc: [Int], map: RecursiveDataStreamMap } in
            let foldRes = foldl (lam arg: Arg. lam node.
                let name = removePrefix node.ident.0 in
                match work false [] [node.body] 0 with { inCount = inCount, acc = subAcc } in
                let subAcc = match (first, semMap) with (true, Some semMap) then
                    match hmLookup name semMap with Some count then
                        if lti (length subAcc) count then
                           parsingWarn (join ["The number of recursive in the sem (", int2string (length subAcc), ") is lower than the counter in the semMap (", int2string count, ")."]);
                           subAcc
                        else
                            subsequence subAcc 0 count
                    else subAcc
                else subAcc
                in
                let map: RecursiveDataStreamMap = hmInsert node.ident.0 (reverse subAcc) arg.map in
                { recLetCount = addi inCount arg.recLetCount, acc = concat subAcc arg.acc, map = map }
            ) { recLetCount = 0, acc = [], map = hashmapEmpty () } bindings in
            
            if first then
               { acc = reverse foldRes.acc, inCount = foldRes.recLetCount, stack = cons inexpr rest, map = foldRes.map }
            else
               let acc = join [foldRes.acc, [foldRes.recLetCount], acc] in
               work false acc (cons inexpr rest) (addi inCount 1) 
        case TmType { inexpr = e } | TmConDef { inexpr = e } then addAndGo [e]
        case TmLet { body = e1, inexpr = e2 } then  addAndGo [e1, e2]
        case TmVar {} | TmConst {} | TmNever {} | TmPlaceholder {} then go []
        case TmLam { body = e } | TmConApp { body = e } | TmExt { inexpr = e } then go [e]
        case TmApp { lhs = e2, rhs = e1 }
           | TmRecordUpdate { rec = e1, value = e2 } then go [e1, e2]
        case TmMatch { target = e1, thn = e2, els = e3 } then go [e1, e2, e3]
        case TmUtest { test = e1, expected = e2, next = e3, tusing = None {}, tonfail = None {} }
             then addAndGo [e1, e2, e3]
        case TmUtest { test = e1, expected = e2, tusing = Some e3, tonfail = None {}, next = e4 }
           | TmUtest { test = e1, expected = e2, tusing = None {}, tonfail = Some e3, next = e4 }
             then addAndGo [e1, e2, e3, e4]
        case TmUtest { test = e1, expected = e2, tusing = Some e3, tonfail = Some e4, next = e5 }
             then addAndGo [e1, e2, e3, e4, e5]
        case TmSeq { tms = arr } then  go arr
        case TmRecord { bindings = map } then  go (mapValues map)
        end
    in
    match work true [] stream.stack 0 with { map = map, acc = acc, inCount = inCount, stack = stack } in
    (match stack with [] then parsingWarn "Stack should not be empty" else ());
    { inCount = inCount, stream = { stream with stack = stack }, map = map, acc = acc }



type RecursiveDataStreamNextRes = { inCount: Int, stream: RecursiveDataStream }

let recursiveDataStreamNext: RecursiveDataStream -> RecursiveDataStreamNextRes = use MExprAst in use MExprPrettyPrint in lam stream.
    match stream.cache with [inCount] ++ cache then
        { inCount = inCount, stream = { stream with cache = cache } } 
    else 
        match recursiveDataStreamComputeNext stream (None {}) with { acc = acc, inCount = inCount, stream = stream } in
        { inCount = inCount, stream = { stream with cache = acc } }


let recursiveDataStreamLang : RecursiveDataStream -> String -> SemMap -> RecursiveDataStream = lam oldStream. lam langName. lam semMap.
    match recursiveDataStreamComputeNext oldStream (Some semMap) with { map = map, stream = stream } in
    match hmKeys map with [h] ++ _ then
        if strStartsWith (concat "v" langName) h then
           (match stream.cache with [] then () else parsingWarn "The cache should be empty at this point");
           { stream with langMap = map, langName = langName, currentSem = "" }
        else oldStream
    else oldStream

let recursiveDataStreamSem : RecursiveDataStream -> String -> RecursiveDataStream = lam stream. lam semName.
    let semName = join ["v", stream.langName, "_", semName] in

    let stream = if eqString "" stream.currentSem then stream else
       { stream with langMap = hmInsert stream.currentSem stream.cache stream.langMap }
    in
    
    match hmLookup semName stream.langMap with Some cache then
        { stream with cache = cache, currentSem = semName }
    else
        parsingWarn (join ["semName ", semName, " is not in the langMap of the lang ", stream.langName, "."]);
        stream
