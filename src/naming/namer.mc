include "./langs-namespace.mc"
include "./name-map.mc"
include "./name-context.mc"
include "../extracting/objects.mc"

let name : Logger -> NamingOptions -> ObjectTree -> NameContext =
    lam log. lam opt. lam objTree.

    let buildUrl : Bool -> String -> String = lam isStdlib. lam namespace. use Formats in
        let ext = concat "." (formatGetExtension opt.fmt) in
        let prefix = if isStdlib then "Stdlib" else "" in
        let link =  strJoin "/" [opt.urlPrefix, prefix, namespace, ext] in
        normalizePath link
    in

    recursive let work : ObjectTree -> NameContext -> Int -> { ctx: NameContext, nextId: Int } = use ObjectKinds in
        lam objTree. lam ctx. lam nextId.
        let default = { ctx = ctx, nextId = nextId} in

        let obj = objTreeObj objTree in
        let children = objTreeChildren objTree in
        let kind = objKind obj in
        

        match kind with ObjUse {} then
             let used = objName obj in
             match langNamespaceGetByName ctx.langNamespaceSet used with Some langNamespace then
                 let useThis : NameMap -> Int -> String -> [String] -> { nameMap: NameMap, nextId: Int} =
                     lam nameMap. lam nextId. lam kind. lam names.
                     foldl (
                         lam acc. lam name.
                         let namespace = join [langNamespace.objNamespace, "/", kind, "-", name] in
                         let url = buildUrl langNamespace.objIsStdlib namespace in
                         let entry = { entry = url, id = acc.nextId, namespace = namespace, isNested = false } in
                         let nameMap = nameMapInsert acc.nameMap name entry in
                         { nameMap = nameMap, nextId = addi nextId 1 }
                     ) { nameMap = nameMap, nextId = nextId } names
                 in
                 let nameMap = ctx.nameMap in
                 match useThis nameMap nextId "syn" langNamespace.syns with { nameMap = nameMap, nextId = nextId } in
                 match useThis nameMap nextId "sem" langNamespace.sems with { nameMap = nameMap, nextId = nextId } in
                 match useThis nameMap nextId "con" langNamespace.cons with { nameMap = nameMap, nextId = nextId } in
                 match useThis nameMap nextId "type" langNamespace.types with { nameMap = nameMap, nextId = nextId } in
                 { ctx = { ctx with nameMap = nameMap }, nextId = nextId }
             else
                 namingWarn (join ["Failed to fetch the ", used, "lang."]);
                 default
    
        else if objKindHasUrl kind then
            let name = objName obj in
            let langNamespace = match kind with ObjLang { parents = parents } then
                let filterIt : (ObjectKind -> Bool) -> [String] =
                    lam keepIt.
                    mapOption (
                        lam child.
                        let obj = objTreeObj child in
                        if keepIt (objKind obj) then
                           Some (objName obj)
                        else
                           None {}
                    ) children
                in

                let langNamespace = {
                    objNamespace = objNamespace obj,
                    objIsStdlib = objIsStdlib obj,

                    parents = [], -- Will be filled in langNamespaceSetBuildNamespace

                    syns = filterIt (lam k. match k with ObjSyn {} then true else false),
                    sems = filterIt (lam k. match k with ObjSem {} then true else false),
                    types = filterIt (lam k. match k with ObjType {} then true else false),
                    cons = filterIt (lam k. match k with ObjCon {} then true else false)
               } in
               let langNamespace = langNamespaceSetBuildNamespace ctx.langNamespaceSet langNamespace parents in
               langNamespaceSetInsert ctx.langNamespaceSet name langNamespace
            else
                ctx.langNamespaceSet
            in
            
            error "todo"
        else error "todo"
    in 

    (work objTree (nameContextEmpty ()) 1).ctx
