include "../global/logger.mc"
include "../global/namespace-utils.mc"

type NameMapEntry a = { entry: a, id: Int, namespace: String, isNested: Bool }
type NameMapBucket a = HashMap String [NameMapEntry a]

type NameMap a = {
    upper: NameMapBucket a,
    lower: NameMapBucket a
}

let nameMapGetBucket : all a. NameMap a -> String ->  String -> { bucket: NameMapBucket a, update: NameMapBucket a -> NameMap a } =
    lam nameMap. lam name. lam namespace. 
    let subnamespace = namespaceGetSubNamespace namespace in

    let upper = { bucket = nameMap.upper, update = lam m. { nameMap with upper = m } } in
    let lower = { bucket = nameMap.lower, update = lam m. { nameMap with lower = m } } in

    if null name then namingWarn "Name map can't handle empty name."; upper
    else if isUpperAlpha (head name) then upper
    else lower

let nameMapEmpty : all a. () -> NameMap a = lam.
{
    upper = hashmapEmpty (),
    lower = hashmapEmpty ()
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

    if isCallerNested then
        let predicate =
            lam entry.
            let entryDomain = namespaceGetDomain entry.namespace in
            or (not entry.isNested)
               (and (idCmp entry.id callerId) (strStartsWith entryDomain callerDomain))
        in
        if isUpperAlpha (head name) then
           lookup predicate nameMap.upper
        else
           lookup predicate nameMap.lower
    else
        let predicate = lam entry. and (not entry.isNested) (idCmp entry.id callerId) in
        if isUpperAlpha (head name) then
           lookup predicate nameMap.upper
        else
           lookup predicate nameMap.lower
        
