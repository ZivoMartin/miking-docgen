include "hashmap.mc"
include "stdlib.mc"

include "../global/util.mc"

-- HashSet of included files
type IncludeSet a =
    {
        baseLoc: String,
        set: HashMap String a,
        prefix: String,
        programStartPos: String
    }

let includeSetNew : all a. String -> IncludeSet a = lam baseLoc.
    { baseLoc = baseLoc, set = hashmapEmpty (), prefix = baseLoc, programStartPos = sysGetCwd () }

let includeSetPrefix : all a. IncludeSet a -> String = lam set. set.prefix

type IncludeSetInsertResult a = { inserted: Bool, includeSet: IncludeSet a, path: String, isStdlib: Bool }

let includeSetInsert : all a. IncludeSet a -> String -> String -> a -> IncludeSetInsertResult a = lam set. lam loc. lam includeContent. lam mapValue.
    match goHere (dirname loc) includeContent with { path = path, isStdlib = isStdlib } in
    match goHere set.programStartPos path with { path = absPath, isStdlib = isStdlib } in

    let set = if isStdlib then set else  { set with prefix = strLongestCommonPrefix (dirname absPath) set.prefix } in
    
    let res = { inserted = false, includeSet = set, isStdlib = isStdlib, path = path } in
    if hmMem path set.set then res
    else { res with includeSet = { set with set = hmInsert path mapValue set.set }, inserted = true }

let includeSetReplace : all a. IncludeSet a -> String -> a -> IncludeSet a = lam set. lam mapKey. lam mapValue.
    { set with set = hmInsert mapKey mapValue set.set }
    
let includeSetGetValue: all a. IncludeSet a -> String -> Option a = lam set. lam key.
    hmLookup key set.set
