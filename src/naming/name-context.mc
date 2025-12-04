include "./langs-namespace.mc"
include "./name-map.mc"
include "./types-namespace.mc"

include "../extracting/objects.mc"

type NameMapValue = {
    url: String,
    obj: Object
}

type NameMap = NameMap NameMapValue

type NameContext = {
    langNamespaceSet: LangNamespaceSet,
    typeNamespaceSet: TypeNamespaceSet,
    nameMap: NameMap
}

let nameContextEmpty : () -> NameContext = lam. {
    langNamespaceSet = namespaceSetEmpty (),
    typeNamespaceSet = namespaceSetEmpty (),
    nameMap = nameMapEmpty ()
}

let nameContextFetchUrl : NameContext -> Object -> String -> Option String =
    lam ctx. lam obj. lam name.
    let namespace = objNamespace obj in
    optionMap (lam v. v.url)
         (nameMapFetch ctx.nameMap name (objId obj) namespace false)

let nameContextFetchObjUrl : NameContext -> Object -> Option String =
    lam ctx. lam obj.
    let namespace = objNamespace obj in
    let name = objName obj in
    optionMap (lam v. v.url)    
         (nameMapFetch ctx.nameMap name (objId obj) namespace true)

let nameContextGetTypeConstructors : NameContext -> Object -> Option [Object] =
    lam ctx.
    typeNamespaceGetTypeConstructors ctx.typeNamespaceSet
    
