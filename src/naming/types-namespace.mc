include "../global/util.mc"
include "../extracting/objects.mc"

type TypeNamespace = {
    typeObj: Object,
    constructors: [Object]
}

type TypeNamespaceSet = NamespaceSet TypeNamespace

let typeNamespaceInsertNewType : TypeNamespaceSet -> Object -> TypeNamespaceSet =
    lam set. lam obj.
    let obj = objWithSourceCode obj (sourceCodeEmpty ()) in
    namespaceSetInsert set (objName obj) { typeObj = obj, constructors = [] }

let typeNamespaceInsertNewCon : TypeNamespaceSet -> Object -> TypeNamespaceSet =
    use ObjectForms in
    lam set. lam obj.
    let obj = objWithSourceCode obj (sourceCodeEmpty ()) in

    match objForm obj with ObjCon { parentType = parentType } then
        match namespaceSetGetByName set parentType with Some typedef then
            let typedef = { typedef with constructors = concat typedef.constructors [obj] } in
            namespaceSetUpdate set (objName typedef.typeObj) typedef
        else
            namingWarn (join ["Type ", parentType, " is not registered in the type nameset."]); set
    else namingWarn "typeNamespaceInsertNewCon only takes in parameter Con arguments."; set

let typeNamespaceGetTypeConstructors : TypeNamespaceSet -> Object -> Option [Object] =
    use ObjectForms in
    lam set. lam obj.
    match objForm obj with ObjType {} then
    let name = objName obj in
    match hmLookup name set.nameMap with Some ids then
        findMap (
            lam id.
            match namespaceSetGetById set id with Some typedef then
                if eqi (objId obj) (objId typedef.typeObj) then
                     Some typedef.constructors
                else None {}
            else namingWarn (join ["Failed to fetch ", int2string id, " from the id map."]); None {}
        ) ids
    else namingWarn (join ["Failed to fetch the constructors of ", name, "."]); None {}
    else namingWarn "typeNamespaceGetTypeConstructors only takes type in parameter."; None {}
