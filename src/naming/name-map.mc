include "../global/util.mc"

type NameMapEntry a = { entry: a, id: Int, namespace: String, isNested: Bool }

type NameMap a = HashMap String [NameMapEntry a]

let nameMapEmpty : all a. () -> NameMap a = lam. hashmapEmpty ()

let nameMapInsert : all a. NameMap a -> String -> NameMapEntry a -> NameMap a = lam nameMap. lam name. lam entry.
    match optionMap (cons entry) (hmLookup name nameMap) with Some entries then
        hmInsert name entries nameMap
    else
        namingWarn (join ["Failed to fetch ", name, "."]);
        nameMap

let nameMapFetch : all a. NameMap a -> String -> Int -> String -> Option a =
    lam nameMap. lam name. lam callerId. lam callerNamespace.
    match strSplitOnce '/' callerNamespace with Some { right = noFile } then
        optionMap (
            lam entries.
                let predicate = if null noFile then
                    lam entry. and (lti entry.id callerId) (not entry.isNested)
                else
                    lam entry. and (lti entry.id callerId) (strStartsWith entry.namespace callerNamespace)
                in
                optionMap (lam entry. Some entry.entry) (find predicate entries)
        ) hmLookup name nameMap 
    else namingWarn "name has no file part."; None {}
