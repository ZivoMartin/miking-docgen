let concatAll = lam arr.
    recursive let concatAll = lam arr.
        match arr with [] then "" else concat (head arr) (concatAll (tail arr)) in concatAll arr
let printLn = lam word. print word; print "\n"
