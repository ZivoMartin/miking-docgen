include "../global/util.mc"
include "../global/logger.mc"

type Id = Int

type NamespaceSet a = {
     idMap: HashMap Int a,
     nameMap: HashMap String [Int],
     nextId: Int
}

let namespaceSetEmpty : all a. () -> NamespaceSet a = lam. {
    idMap = hashmapEmpty (),
    nameMap = hashmapEmpty (),
    nextId = 1
}

let namespaceSetNameToId : all a. NamespaceSet a -> String -> Option Id =
    lam set. lam name.
    optionMap head (hmLookup name set.nameMap)

let namespaceSetGetById : all a. NamespaceSet a -> Id -> Option a =
    lam set. lam id.
    hmIntLookup id set.idMap

let namespaceSetGetByName : all a. NamespaceSet a -> String -> Option a =
    lam set. lam name.
    optionJoin
    (optionMap (
        lam id.
        match namespaceSetGetById set id with Some namespace then
            Some namespace
        else
            namingWarn (join ["The element ", name, " exists in the name map but not in the id map."]);
            None {}
    ) (namespaceSetNameToId set name))


let namespaceSetInsert : all a. NamespaceSet a -> String -> a -> NamespaceSet a =
    lam set. lam name. lam namespace.

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
