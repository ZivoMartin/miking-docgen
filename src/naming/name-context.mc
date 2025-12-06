include "./langs-namespace.mc"
include "./name-map.mc"
include "./types-namespace.mc"

include "../extracting/objects.mc"

let buildUrl : use Formats in String -> String -> Format -> Bool -> String -> String =
    use Formats in
    lam stdlibFolder. lam urlPrefix. lam fmt. lam isStdlib. lam namespace. 
    let ext = concat "." (formatGetExtension fmt) in
    let prefix = if isStdlib then stdlibFolder  else "" in
    let link =  strJoin "/" [urlPrefix, prefix, concat namespace ext] in
    normalizePath link


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

let nameContextFetch : NameContext -> Object -> String -> Option NameMapValue =
    lam ctx. lam obj. lam name.
    let namespace = objNamespace obj in
    nameMapFetch ctx.nameMap name (objId obj) namespace false

let nameContextGetTypeConstructors : NameContext -> Object -> Option [Object] =
    lam ctx.
    typeNamespaceGetTypeConstructors ctx.typeNamespaceSet
    
