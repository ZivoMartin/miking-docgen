include "../doc-tree.mc"
include "../token-readers.mc"

-- let changeExt : (String -> String -> String) = lam fileName. lam ext.
--     match findiLast (eqc '.') fileName with Some i then
--         concat (subsequence fileName 0 (addi 1 i)) ext
--     else
--         concat fileName (cons '.' ext)

let getId =
    lam namespace. lam name.
    concatAll [name, "~", namespace]


let getNamespace = lam namespace. lam name.
    concatAll [namespace, "-", name]

    
let extractLastNamespaceElement = lam namespace.
    recursive let extractLastNamespaceElement = lam namespace.
        match namespace with "-" ++ namespace then
            extractLastNamespaceElement namespace
        else match namespace with [x] ++ namespace then
            let res = extractLastNamespaceElement namespace in
            if res.1 then (cons x res.0, res.1)
            else res
        else ([], true)
    in (extractLastNamespaceElement namespace).0 

let sanitizePath = lam path.
    recursive
    let sanitizePath = lam path.
        switch path
        case (['/'] ++ path) then cons '-' (sanitizePath path)
        case [x] ++ path then cons x (sanitizePath path)
        case [] then [] end
    in sanitizePath path

let removeComments = use TokenReader in
    lam sons. filter (lam s. match s with Leaf { token = Comment {} } then false else true) sons

let nthWord = use TokenReader in lam sons. lam n.
    recursive
    let nthWord = lam sons. lam n.
        switch sons
        case [Leaf { token = Word { content = word } }] ++ rest then
            if eqi n 0 then Some (word, rest)
            else nthWord rest (subi n 1)
        case [_] ++ rest then nthWord rest n
        case [] then None {}
        end in
    nthWord sons n


let extractType = use TokenReader in lam typedef.             
     foldl (lam s. lam t.
         switch t
         case Leaf {token = Word { content = "in" } } then s
         case Leaf {token = Word { content = content } } then concat content s
         case _ then s end
          ) "" (reverse typedef) 
