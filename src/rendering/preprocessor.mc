-- # Preprocess step: create output directories
--
-- This module implements the preprocessing step:
-- - Walks the ObjectTree
-- - Computes all the output directories needed to generate doc files
-- - Creates them using `mkdir -p`
--
-- It builds a PathMap and runs a system command at the end.

include "../extracting/objects.mc"
include "../util.mc"
include "fileutils.mc"
include "hashmap.mc"

let preprocess : ObjectTree -> () = lam obj.
    -- Map of all output paths (acts as a Set)
    type PathMap = HashMap String () in

    -- Recursively visit the ObjectTree and collect paths
    recursive let preprocessRec : PathMap -> ObjectTree -> PathMap = use ObjectKinds in
        lam pathMap. lam obj.
        switch obj
        case ObjectNode { obj = { kind = ObjInclude {} }, sons = [ p ] } then preprocessRec pathMap p
        case ObjectNode { obj = { kind = ObjUse {} | ObjInclude {} }, sons = sons } then pathMap
        case ObjectNode { obj = obj, sons = sons } then
            let path = dirname (concat "doc-gen-output/" (objLink obj)) in
            foldl preprocessRec (hmInsert path () pathMap) sons

        case _ then pathMap end

    in
    let pathMap = preprocessRec (hashmapEmpty ()) obj in
    let command = concat ["mkdir", "-p"] (hmKeys pathMap) in
    let res = sysRunCommand command "" "." in ()
