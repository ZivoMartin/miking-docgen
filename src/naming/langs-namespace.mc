include "../global/util.mc"

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


type LangNamespaceSet = {
     idMap: HashMap Int LangNamespace,
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
    hmIntLookup id set.idMap

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
    lam set. lam name. lam namespace.

    let parents = map
        (lam parent.
             match langNamespaceGetById set parent with Some namespace then
                 namespace
             else
                 namingWarn "Failed to fetch the parent lang.";
                 langNamespaceDefault
        ) namespace.parents in

    let unite : (LangNamespace -> [String]) -> [String] = lam getter.
        let union = foldl (lam acc. lam from.
                let field = getter from in
                foldl (lam acc. lam arg. hmInsert arg () acc) acc field
            ) (hashmapEmpty ()) (cons namespace parents) in
        hmKeys union
    in
    
    let namespace = { namespace with 
         syns = unite (lam namespace. namespace.syns),
         sems = unite (lam namespace. namespace.sems),
         types = unite (lam namespace. namespace.types),
         cons = unite (lam namespace. namespace.cons)
    } in

    let id = set.nextId in
    let idSet =
        match hmLookup name set.nameMap with Some idSet then
            cons id idSet
        else [id] 
    in

    { set with
      nextId = addi 1 id,
      idMap = hmIntInsert id namespace set.idMap,
      nameMap = hmInsert name idSet set.nameMap
    }
