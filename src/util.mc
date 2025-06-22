include "string.mc"
include "hashmap.mc"

-- Takes an array of String and concatenates then into one single String
let concatAll = lam arr.
    recursive let concatAll = lam arr.
        match arr with [] then "" else concat (head arr) (concatAll (tail arr)) in concatAll arr
    
let printLn = lam word. print word; print "\n"

let contains = lam arr. lam lword. 
    recursive
    let contains = lam arr. lam lword.
        match arr with [] then
            false
        else
            or (eqString (head arr) lword) (contains (tail arr) lword)
    in contains arr lword

let strContains = lam s. lam c. match find (lam x. eqc c x) s with Some _ then true else false

let repeat = lam s. lam n.
    recursive let repeat = lam s. lam n.
        if eqi n 0 then []
        else cons s (repeat s (subi n 1))
    in repeat s n


let changeExt : (String -> String -> String) = lam fileName. lam ext.
    match findiLast (eqc '.') fileName with Some i then
        concat (subsequence fileName 0 (addi 1 i)) ext
    else
        concat fileName (cons '.' ext)

let normalizePath = lam path.
    let isAbsolute = match path with "/" ++ s then true else false in
    let components = strSplit "/" path in
    recursive let process = lam comps. lam stack.
        switch comps
        case [] then stack
        case ["."] ++ rest then process rest stack
        case [""] ++ rest then process rest stack  -- skip multiple slashes
        case [".."] ++ rest then
            (switch stack
             case ([] | [".."] ++ _) then process rest (cons ".." stack)
             case [_] ++ tl then process rest tl end)
        case [comp] ++ rest then process rest (cons comp stack) end
    in
    let cleaned = reverse (process components []) in
    let result = strJoin "/" cleaned in
    if isAbsolute then cons '/'  result
    else result

utest normalizePath "repo1/../repo2" with "repo2"
utest normalizePath "/repo1/../repo2" with "/repo2"
utest normalizePath "../../repo2" with "../../repo2"
utest normalizePath "./a/./b/../c" with "a/c"
utest normalizePath "/a/b/../../c" with "/c"


let flip : all a. all b. all c. (a -> b -> c) -> (b -> a -> c) =
  lam f. lam b. lam a. f a b

let strTruncate = lam s. lam n.
    recursive let strTruncate = lam str. lam n.
        match str with [_] ++ s then
            if eqi n 0 then str
            else strTruncate s (subi n 1)
        else str in strTruncate s n

let hmTraits = hashmapStrTraits
let hmInsert = lam x. hashmapInsert hmTraits x
let hmMem = lam x. hashmapMem hmTraits x
let hmValues = lam x. hashmapValues hmTraits x
let hmKeys = lam x. hashmapKeys hmTraits x    
