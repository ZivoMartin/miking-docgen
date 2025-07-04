-- # Small string & hashmap utilities
--
-- A collection of helper functions:
--
-- These utilities simplify common operations used across other modules.

include "string.mc"
include "hashmap.mc"
include "sys.mc"

-- Print a line with newline
let printLn: String -> () = lam word. print word; print "\n"; flushStdout () 
        
-- Takes an array of String and concatenates them into one single String
let concatAll = lam arr. strJoin "" arr

-- Check if an array of strings contains lword
recursive let contains = lam arr. lam lword.
    match arr with [] then
        false
    else
        or (eqString (head arr) lword) (contains (tail arr) lword)
end

-- Check if string s contains character c
let strContains = lam s. lam c. match find (lam x. eqc c x) s with Some _ then true else false

-- Repeat string s, n times (returns an array)

recursive let repeat = lam s. lam n.
    if eqi n 0 then []
    else cons s (repeat s (subi n 1))
end

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

recursive let strTruncate = lam str. lam n.
    match str with [_] ++ s then
        if eqi n 0 then str
        else strTruncate s (subi n 1)
    else str
end

let splitOnL : all a. (a -> Bool) -> [a] -> { left: [a], right: [a] } = lam f. lam arr.
    recursive let work = lam arr.
        switch arr
        case [] then { left = [], right = [] }
        case [x] ++ rest then
            if f x then
                { left = [x], right = rest }      
            else
                let res = work rest in
                { res with left = cons x res.left }
        end in
    work arr
    
let splitOnR : all a. (a -> Bool) -> [a] -> { left: [a], right: [a] } = lam f. lam arr.
    recursive let work = lam arr.
        switch arr
        case [] then { left = [], right = [] }
        case [x] ++ rest then
            if f x then
                { left = [], right = arr }      
            else
                let res = work rest in
                { res with left = cons x res.left }
        end in
    work arr

    
-- HashMap helpers for String-based maps

let hmTraits = hashmapStrTraits
let hmInsert = lam x. hashmapInsert hmTraits x
let hmMem = lam x. hashmapMem hmTraits x
let hmValues = lam x. hashmapValues hmTraits x
let hmKeys = lam x. hashmapKeys hmTraits x

    
