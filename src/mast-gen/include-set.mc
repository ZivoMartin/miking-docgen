-- # IncludeSet Module
--
-- The IncludeSet provides utilities to track which files have been visited
-- during code parsing/lexing. It stores each file name in a hashmap as a key,
-- mapped with a value provided as a parameter. It is later used to map each
-- path to its AST.
--
-- When inserting a new name, the IncludeSet resolves the actual full path of
-- the file from a given location. It also computes, throughout its lifetime, a
-- common prefix of all resolved paths to help reduce page URL sizes.

include "hashmap.mc"
include "stdlib.mc"
include "../global/util.mc"

-- A set of included files with metadata.
type IncludeSet a = HashMap String a

-- Creates a new IncludeSet with a given base location.
let includeSetNew : all a. () -> IncludeSet a = lam.
    hashmapEmpty ()

-- Result type for inserting a new element in the IncludeSet.
type IncludeSetInsertResult a = {
     inserted: Bool,
     includeSet: IncludeSet a,
     path: String,
     isStdlib: Bool
}

-- Inserts a file path into the IncludeSet
let includeSetInsert : all a. IncludeSet a -> String -> String -> a -> IncludeSetInsertResult a =
    lam set. lam loc. lam includeContent. lam mapValue.

    match goHere (dirname loc) includeContent with { path = path, isStdlib = isStdlib } in

    let res = { inserted = false, includeSet = set, isStdlib = isStdlib, path = path } in

    if hmMem path set then res
    else { res with includeSet = hmInsert path mapValue set, inserted = true }

-- Replaces or inserts a value in the IncludeSet with the given key.
let includeSetReplace : all a. IncludeSet a -> String -> a -> IncludeSet a = lam set. lam mapKey. lam mapValue.
    hmInsert mapKey mapValue set
    
-- Looks up a value in the IncludeSet by key.
let includeSetGetValue: all a. IncludeSet a -> String -> Option a = lam set. lam key.
    hmLookup key set
