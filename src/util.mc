-- # Small string & hashmap utilities
--
-- A collection of helper functions:
--
-- These utilities simplify common operations used across other modules.

include "string.mc"
include "hashmap.mc"

-- Takes an array of String and concatenates them into one single String
let concatAll = lam arr. strJoin "" arr

-- Print a line with newline
let printLn = lam word. print word; print "\n"

-- Check if an array of strings contains lword
let contains = lam arr. lam lword. 
    recursive
    let contains = lam arr. lam lword.
        match arr with [] then
            false
        else
            or (eqString (head arr) lword) (contains (tail arr) lword)
    in contains arr lword

-- Check if string s contains character c
let strContains = lam s. lam c. match find (lam x. eqc c x) s with Some _ then true else false

-- Repeat string s, n times (returns an array)
let repeat = lam s. lam n.
    recursive let repeat = lam s. lam n.
        if eqi n 0 then []
        else cons s (repeat s (subi n 1))
    in repeat s n

-- Change the extension of a filename
let changeExt : (String -> String -> String) = lam fileName. lam ext.
    match findiLast (eqc '.') fileName with Some i then
        concat (subsequence fileName 0 (addi 1 i)) ext
    else
        concat fileName (cons '.' ext)

-- Flip arguments of a function
let flip : all a. all b. all c. (a -> b -> c) -> (b -> a -> c) =
    lam f. lam b. lam a. f a b

-- Truncate the first n characters from a string
let strTruncate = lam s. lam n.
    recursive let strTruncate = lam str. lam n.
        match str with [_] ++ s then
            if eqi n 0 then str
            else strTruncate s (subi n 1)
        else str in strTruncate s n

-- HashMap helpers for String-based maps

let hmTraits = hashmapStrTraits
let hmInsert = lam x. hashmapInsert hmTraits x
let hmMem = lam x. hashmapMem hmTraits x
let hmValues = lam x. hashmapValues hmTraits x
let hmKeys = lam x. hashmapKeys hmTraits x
