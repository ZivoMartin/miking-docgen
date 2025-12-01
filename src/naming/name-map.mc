    include "../global/logger.mc"
include "../global/namespace-utils.mc"

type NameMapEntry a = { entry: a, id: Int, namespace: String, isNested: Bool }

type NameMap a = HashMap String [NameMapEntry a]

let nameMapEmpty : all a. () -> NameMap a = lam. hashmapEmpty ()

let nameMapInsert : all a. NameMap a -> String -> NameMapEntry a -> NameMap a = lam nameMap. lam name. lam entry.
    match optionMap (cons entry) (hmLookup name nameMap) with Some entries then
        hmInsert name entries nameMap
    else
        hmInsert name [entry] nameMap

let nameMapFetch : all a. NameMap a -> String -> Int -> String -> Bool -> Option a =
    lam nameMap. lam name. lam callerId. lam callerNamespace. lam me.
    let isNested = namespaceIsNested callerNamespace in
    let callerDomain = namespaceGetDomain callerNamespace in

    let idCmp = if me then leqi else lti in

    let res = optionMap (
        lam entries.
            let predicate = if not isNested then
                lam entry. and (idCmp entry.id callerId) (not entry.isNested)
            else
                lam entry.
                    let entryDomain = namespaceGetDomain entry.namespace in
                    and (idCmp entry.id callerId) (strStartsWith entryDomain callerDomain)
            in
            optionMap (lam entry. entry.entry) (find predicate entries)
    ) (hmLookup name nameMap) in
    optionJoin res
