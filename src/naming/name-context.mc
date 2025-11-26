include "./langs-namespace.mc"
include "./name-map.mc"
include "../extracting/objects.mc"

type NameMap = NameMap String

type NameContext = {
    langNamespaceSet: LangNamespaceSet,
    nameMap: NameMap
}

let nameContextEmpty : () -> NameContext = lam. {
    langNamespaceSet = langNamespaceSetEmpty (),
    nameMap = nameMapEmpty ()
}

let nameContextFetchUrl : NameContext -> Object -> String -> Option String =
    lam ctx. lam obj. lam name.
    let namespace = objNamespace obj in
    nameMapFetch ctx.nameMap name (objId obj) namespace false

let nameContextFetchObjUrl : NameContext -> Object -> Option String =
    lam ctx. lam obj.
    let namespace = objNamespace obj in
    let name = objName obj in
    nameMapFetch ctx.nameMap name (objId obj) namespace true
