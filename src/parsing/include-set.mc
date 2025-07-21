include "hashmap.mc"
include "stdlib.mc"

include "../global/util.mc"

-- HashSet of included files
type IncludeSet =
    {
        baseLoc: String,
        set: HashMap String (),
        prefix: String
    }

let includeSetNew : String -> IncludeSet = lam baseLoc.
    { baseLoc = baseLoc, set = hashmapEmpty (), prefix = baseLoc }

let includeSetPrefix : IncludeSet -> String = lam set. set.prefix

type IncludeSetInsertResult = { inserted: Bool, includeSet: IncludeSet, path: String, isStdlib: Bool }

let includeSetInsert : IncludeSet -> String -> String -> IncludeSetInsertResult = lam set. lam loc. lam includeContent.
    match goHere (dirname loc) includeContent with { path = path, isStdlib = isStdlib } in
    match goHere set.baseLoc path with { path = absPath, isStdlib = isStdlib } in

    let set = if isStdlib then set else  { set with prefix = strLongestCommonPrefix (dirname absPath) set.prefix } in
    
    let res = { inserted = false, includeSet = set, isStdlib = isStdlib, path = path } in
    if hmMem path set.set then res
    else { res with includeSet = { set with set = hmInsert path () set.set }, inserted = true }

 
