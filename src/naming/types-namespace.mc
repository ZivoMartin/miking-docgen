include "../global/util.mc"
include "../extracting/objects.mc"

type TypeNamespace = {
    typeObj: Object,
    constructors: [Object]
}

type TypeNamespaceSet = NamespaceSet TypeNamespace

let typeNamespaceInsertNewType : TypeNamespaceSet -> Object -> TypeNamespaceSet =
    lam set. lam obj.
    namespaceSetInsert set (objName obj) { typeObj = obj, constructors = [] }

let typeNamespaceInsertNewCon : TypeNamespaceSet -> Object -> TypeNamespaceSet =
    use ObjectKinds in
    lam set. lam obj.
    match objKind obj with ObjCon { parentType = parentType } then
        match namespaceSetGetByName set parentType with Some typedef then
            let typedef = { typedef with constructors = cons obj typedef.constructors } in
            namespaceSetUpdate set (objName typedef.typeObj) typedef
        else
            namingWarn (join ["Type ", parentType, " is not registered in the type nameset."]); set
    else namingWarn "typeNamespaceInsertNewCon only takes in parameter Con arguments."; set
