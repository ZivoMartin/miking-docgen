-- # Preprocess step: create output directories
--
-- This module implements the preprocessing step:
-- - Walks the ObjectTree
-- - Computes all the output directories needed to generate doc files
-- - Creates them using `mkdir -p`
--
-- It builds a PathMap and runs a system command at the end.

include "../extracting/objects.mc"
include "./renderers/objects-renderer.mc"
include "../global/util.mc"
include "fileutils.mc"
include "hashmap.mc"
include "../options/options.mc"
include "../global/format.mc"    
include "./name-context.mc"

let preprocess : ObjectTree -> RenderingOptions -> () = use ObjectsRenderer in lam obj. lam opt.
    -- Map of all output paths (acts as a Set)
    type PathMap = HashMap String () in
    -- Recursively visit the ObjectTree and collect paths
    recursive let preprocessRec : Int -> PathMap -> ObjectTree -> PathMap = use ObjectKinds in
        lam depth. lam pathMap. lam obj.
        let inner = objTreeObj obj in
        let opt = { opt with nameContext = nameContextInsertIfHas opt.nameContext inner (objGetPureLink inner opt) } in
        switch obj
        case ObjectNode { obj = { kind = ObjInclude {} } & obj, sons = [ p ] } then
            if and (objIsStdlib obj) opt.noStdlib then pathMap else
                preprocessRec 0 pathMap p
        case ObjectNode { obj = { kind = ObjUse {} | ObjInclude {} }, sons = sons } then pathMap
        case ObjectNode { obj = obj, sons = sons } then
            let path = dirname (join [opt.outputFolder, objLink obj opt]) in
            let map = hmInsert path () pathMap in
            let go = lam depth. lam sons. foldl (preprocessRec depth) map sons in
            switch obj.kind
            case ObjSem {} | ObjLet {} then
                let depth = addi 1 depth in
                if optionEq and (Some true) (optionMap (gti depth) opt.letDepth) then -- depth > letDepth ?
                    pathMap
                else go depth sons
            case _ then go depth sons
            end
            
        case _ then pathMap end

    in
    let pathMap = preprocessRec 0 (hashmapEmpty ()) obj in
    recursive let create = lam arr.
        let batchSize = 1000 in
        match arr with [] then ()
        else
            let arr = if lti (length arr) batchSize then (arr, []) else splitAt arr batchSize in
            let command = concat ["mkdir", "-p"] arr.0 in
            let res = sysRunCommand command "" "." in
            match res.returncode with 0 then create arr.1 else error "Failed to create folders during preprocessing"
    in create (hmKeys pathMap)
