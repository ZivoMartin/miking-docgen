include "string.mc"

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

let contains = lam arr. lam lword. 
    recursive
    let contains = lam arr. lam lword.
        match arr with [] then
            false
        else
            or (eqString (head arr) lword) (contains (tail arr) lword)
    in contains arr lword

