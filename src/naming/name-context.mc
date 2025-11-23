include "./langs-namespace.mc"
include "./name-map.mc"

type NameMap = NameMap String

type NameContext = {
    langNamespaceSet: LangNamespaceSet,
    nameMap: NameMap
}

let nameContextEmpty : all a. () -> NameContext = lam. {
    langNamespaceSet = langNamespaceSetEmpty (),
    nameMap = nameMapEmpty ()
}
