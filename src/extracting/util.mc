include "../parsing/doc-tree.mc"
include "../parsing/token-readers.mc"
include "../util.mc"
include "string.mc"
include "stdlib.mc"
include "sys.mc"

let getNamespace = lam namespace. lam name.
    concatAll [namespace, "/", name]

let extractLastNamespaceElement = lam namespace.
    match strSplit "/" namespace with ([_] ++ _) & namespace then head (reverse namespace) else ""

let sanitizePath = lam path.
    -- recursive
    -- let sanitizePath = lam path.
    --     switch path
    --     case (['/'] ++ path) then cons '-' (sanitizePath path)
    --     case [x] ++ path then cons x (sanitizePath path)
    --     case [] then [] end
    -- in sanitizePath path
    path

let goHere = lam currentLoc. lam target.
    let path = if strStartsWith "/" target then target
               else concatAll [currentLoc, "/", target] in
    if sysFileExists path then path else concatAll [stdlibLoc, "/", target]

        
let removeComments = use TokenReader in
    lam sons. filter (lam s. match s with Leaf { token = Comment {} } then false else true) sons

let nthWord = use TokenReader in lam sons. lam n.
    recursive
    let nthWord = lam sons. lam n.
        switch sons
        case [Leaf { token = Word { content = word } }] ++ rest then
            if eqi n 0 then Some { word = word, rest = rest }
            else nthWord rest (subi n 1)
        case [_] ++ rest then nthWord rest n
        case [] then None {}
        end in
    nthWord sons n

let extractType = use TokenReader in lam typedef.     
     foldl (lam s. lam t.
         switch t
         case Leaf {token = Word { content = "in" } } then s
         case Leaf {token = Word { content = "," } } then concat "," s
         case Leaf {token = Word { content = content } } then concatAll [" ", content, s]
         case _ then s end
          ) "" (reverse typedef)

let extractParents = lam words.
    recursive let extractParents = lam words.
        match nthWord words 0 with Some { word = w, rest = words } then
            switch w
            case "end" | "type" | "sem" | "syn" | "con" then []
            case "=" | "+" then extractParents words
            case name then cons name (extractParents words)
            end
        else [] in extractParents words

-- Skip all the use name in pattern and returns the stream.
let skipUseIn : [DocTree] -> [DocTree] = lam sons.
    recursive let skipUseIn = lam sons.
        match nthWord sons 0 with
        Some { word = "use", rest = sons } then
            match (nthWord sons 1) with Some { rest = sons } then
                skipUseIn sons else never
        else sons in skipUseIn sons
