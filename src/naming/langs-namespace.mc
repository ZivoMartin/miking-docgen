include "../global/util.mc"
include "../global/logger.mc"

type LangId = Int

-- For now we only store the names.
type LangNamespace = {
     objNamespace: String,
     objIsStdlib: Bool,
     
     parents: [LangId],

     syns: [String],
     sems: [String],
     types: [String],
     cons: [String]
}


let langNamespaceDefault : LangNamespace = {
    objNamespace = "",
    objIsStdlib = false,

    parents = [],

    syns = [],
    sems = [],
    types = [],
    cons = []
}

type LangNamespaceDatas = {
    -- The full rebuilt namespace
    full: LangNamespace,
    -- Only what was already in the namespace
    explicit: LangNamespace,
    -- Only what have been added
    implicit: LangNamespace
}

type LangNamespaceSet = {
     idMap: HashMap Int LangNamespaceDatas,
     nameMap: HashMap String [Int],
     nextId: Int
}

let langNamespaceSetEmpty : () -> LangNamespaceSet = lam. {
    idMap = hashmapEmpty (),
    nameMap = hashmapEmpty (),
    nextId = 1
}

let langNamespaceNameToId : LangNamespaceSet -> String -> Option LangId =
    lam set. lam name.
    optionMap head (hmLookup name set.nameMap)

let langNamespaceSetBuildNamespace : LangNamespaceSet -> LangNamespace -> [String] -> LangNamespace =
    lam set. lam langNamespace. lam parents.
    let parents = map (lam langName.
        match langNamespaceNameToId set langName with Some id then id
        else namingWarn "There is a non existing lang in the parent list."; 0) parents in
    { langNamespace with parents = parents }

let langNamespaceGetById : LangNamespaceSet -> LangId -> Option LangNamespace =
    lam set. lam id.
    optionMap (lam d. d.full) (hmIntLookup id set.idMap)

let langNamespaceGetByName : LangNamespaceSet -> String -> Option LangNamespace =
    lam set. lam name.
    optionMap (
        lam id.
        match langNamespaceGetById set id with Some namespace then
            namespace
        else
            namingWarn (join ["The lang ", name, " exists in the name map but not in the id map."]);
            langNamespaceDefault
    ) (langNamespaceNameToId set name)


let langNamespaceSetInsert : LangNamespaceSet -> String -> LangNamespace -> LangNamespaceSet =
    lam set. lam name. lam explicit.

    let parents = map
        (lam parent.
             match langNamespaceGetById set parent with Some namespace then
                 namespace
             else
                 namingWarn "Failed to fetch the parent lang.";
                 langNamespaceDefault
        ) explicit.parents in

    let unite : (LangNamespace -> [String]) -> [String] = lam getter.
        let union = foldl (lam acc. lam from.
                let field = getter from in
                foldl (lam acc. lam arg. hmInsert arg () acc) acc field
            ) (hashmapEmpty ()) (cons explicit parents) in
        hmKeys union
    in

    let diff : (LangNamespace -> [String]) -> [String] = lam getter.
        let explicit = foldl (lam acc. lam arg. hmInsert arg () acc) (hashmapEmpty ()) (getter explicit) in
        
        let diff = foldl (lam acc. lam from.
                let field = getter from in
                foldl (lam acc. lam arg.
                    match hmLookup arg explicit with None {} then
                        hmInsert arg () acc
                    else acc
                ) acc field
            ) (hashmapEmpty ()) parents in
        hmKeys diff
    in
    
    let full = { explicit with 
         syns = unite (lam namespace. namespace.syns),
         sems = unite (lam namespace. namespace.sems),
         types = unite (lam namespace. namespace.types),
         cons = unite (lam namespace. namespace.cons)
    } in

    let implicit = { explicit with 
         syns = diff (lam namespace. namespace.syns),
         sems = diff (lam namespace. namespace.sems),
         types = diff (lam namespace. namespace.types),
         cons = diff (lam namespace. namespace.cons)
    } in

    let id = set.nextId in
    let idSet =
        match hmLookup name set.nameMap with Some idSet then
            cons id idSet
        else [id] 
    in

    let datas = {
        explicit = explicit,
        full = full,
        implicit = implicit
    } in

    { set with
      nextId = addi 1 id,
      idMap = hmIntInsert id datas set.idMap,
      nameMap = hmInsert name idSet set.nameMap
    }

let langNamespaceGetAddedChildren : LangNamespaceSet -> String -> Option LangNamespace =
    lam set. lam name.
    optionJoin
        (optionMap
        (lam id. optionMap (lam d. d.implicit) (hmIntLookup id set.idMap))
        (langNamespaceNameToId set name))
        
