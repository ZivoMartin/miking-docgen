include "../global/logger.mc"
include "../global/namespace-utils.mc"

type NameMapEntry a = { entry: a, id: Int, namespace: String, isNested: Bool }
type NameMapBucket a = HashMap String [NameMapEntry a]

type NameMap a = {
    rootUpper: NameMapBucket a,
    rootLower: NameMapBucket a,
    nestedUpper: NameMapBucket a,
    nestedLower: NameMapBucket a
}

let nameMapGetBucket : all a. NameMap a -> String ->  String -> { bucket: NameMapBucket a, update: NameMapBucket a -> NameMap a } =
    lam nameMap. lam name. lam namespace. 
    let subnamespace = namespaceGetSubNamespace namespace in
    if null name then
        namingWarn "Name map can't handle empty name.";
        { bucket = nameMap.rootUpper, update = lam m. { nameMap with rootUpper = m } }
    else if isUpperAlpha (head name) then
        if isNested then
           { bucket = nameMap.nestedUpper, update = lam m. { nameMap with nestedUpper = m } }
        else
           { bucket = nameMap.rootUpper, update = lam m. { nameMap with rootUpper = m } }
    else
        if isNested then
            { bucket = nameMap.nestedLower, update = lam m. { nameMap with nestedLower = m } }
        else
            { bucket = nameMap.rootLower, update = lam m. { nameMap with rootLower = m } }
       

let nameMapEmpty : all a. () -> NameMap a = lam.
{
    rootUpper = hashmapEmpty (),
    rootLower = hashmapEmpty (),
    nestedUpper = hashmapEmpty (),
    nestedLower = hashmapEmpty ()
}

let nameMapInsert : all a. NameMap a -> String -> String -> NameMapEntry a -> NameMap a =
    lam nameMap. lam name. lam namespace. lam entry.

    match nameMapGetBucket nameMap name namespace with { bucket = oldBucket, update = update } in

    let newBucket = match optionMap (cons entry) (hmLookup name oldBucket) with Some entries then
        hmInsert name entries oldBucket
    else
        hmInsert name [entry] oldBucket
    in

     update newBucket


let nameMapFetch : all a. NameMap a -> String -> Int -> String -> Bool -> Option a =
    lam nameMap. lam name. lam callerId. lam callerNamespace. lam me.
    if null name then None {} else

    let isCallerNested = namespaceIsNested callerNamespace in
    let callerDomain = namespaceGetDomain callerNamespace in
    let idCmp = if me then leqi else lti in

    let lookup = lam predicate. lam bucket.
        let res = optionMap (
            lam entries.
                optionMap (lam entry. entry.entry) (find predicate entries)
        ) (hmLookup name bucket) in
        optionJoin res
    in

    let nonNestedFetch: Option a =
        let predicate = lam entry. idCmp entry.id callerId in
        if isUpperAlpha (head name) then
           lookup predicate nameMap.rootUpper
        else
           lookup predicate nameMap.rootLower
    in    

    if isCallerNested then
        optionOrElse
            (lam.
               let predicate =
                   lam entry.
                   let entryDomain = namespaceGetDomain entry.namespace in
                   and (idCmp entry.id callerId) (strStartsWith entryDomain callerDomain)
               in
               if isUpperAlpha (head name) then
                  lookup predicate nameMap.nestedUpper
               else
                  lookup predicate nameMap.nestedLower)
            nonNestedFetch
    else nonNestedFetch
        
