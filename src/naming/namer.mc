include "./langs-namespace.mc"
include "./name-map.mc"
include "./name-context.mc"
include "../extracting/objects.mc"

type NamingRes = {
     annotatedObjTree: ObjectTree,
     nameContext: NameContext
}

let name : Logger -> NamingOptions -> ObjectTree -> NamingRes =
    lam log. lam opt. lam objTree.

    let buildUrl : Bool -> String -> String = lam isStdlib. lam namespace. use Formats in
        let ext = concat "." (formatGetExtension opt.fmt) in
        let prefix = if isStdlib then "Stdlib" else "" in
        let link =  strJoin "/" [opt.urlPrefix, prefix, concat namespace ext] in
        normalizePath link
    in

    type WorkRes = { ctx: NameContext, nextId: Int, objTree: ObjectTree } in
    recursive let work : ObjectTree -> NameContext -> Int -> WorkRes = use ObjectKinds in
        lam objTree. lam ctx. lam nextId. 


        let obj = objTreeObj objTree in
        let children = objTreeChildren objTree in
        let kind = objKind obj in

        let isNested = namespaceIsNested (objNamespace obj) in

        let process : NameContext -> ObjectTree -> Int -> [ObjectTree] -> WorkRes =
            lam ctx. lam objTree. lam nextId. lam children.
            let obj = objTreeObj objTree in

            let res = foldl (
                lam acc. lam child.
                let children = objTreeChildren acc.objTree in
                let res = work child acc.ctx acc.nextId in
                { res with ctx = res.ctx, objTree = ObjectNode { obj = obj, children = cons res.objTree children }}
            ) { nextId = nextId, ctx = ctx, objTree = ObjectNode { children = [], obj = obj } } children
            in

            { res with objTree = ObjectNode { obj = obj, children = reverse (objTreeChildren res.objTree) } }
        in
        
        let annotate : NameContext -> ObjectTree -> Int -> [ObjectTree] -> WorkRes =
            lam ctx. lam objTree. lam nextId. lam children.

            let children = objTreeChildren objTree in
            let obj = objTreeObj objTree in
            let obj = objWithId obj nextId in
            let nextId = addi 1 nextId in
            let objTree = ObjectNode { obj = obj, children = children } in

            let ctx = 
                if objKindHasUrl kind then
                    let name = objName obj in
                    let namespace = objNamespace obj in
                    let isStdlib = objIsStdlib obj in

                    let url = buildUrl isStdlib namespace in
                    let entry = { entry = url, id = objId obj, namespace = namespace, isNested = isNested } in

                    log (join
                        ["Adding ", name, " in the name map.\n",
                        "namespace=", entry.namespace, "\n",
                        "url=", entry.entry, "\n",
                        "id=", int2string entry.id, "\n",
                        "isNested=", bool2string entry.isNested, "\n"]);

                    let nameMap = nameMapInsert ctx.nameMap name entry in
                    { ctx with nameMap = nameMap }
                else ctx
            in            
            { ctx = ctx, nextId = nextId, objTree = objTree }
        in

        let annotateAndProcess : NameContext -> Int -> [ObjectTree] -> WorkRes =
            lam ctx. lam nextId. lam children.
            match annotate ctx objTree nextId children with
            { ctx = ctx, nextId = nextId, objTree = objTree } in
            process ctx objTree nextId children
        in

        let processAndAnnotate : NameContext -> Int -> [ObjectTree] -> WorkRes =
            lam ctx. lam nextId. lam children.
            match process ctx objTree nextId children with
            { ctx = ctx, nextId = nextId, objTree = objTree } in
            annotate ctx objTree nextId children
        in


        -- We first insert the direct children, then we call process. So direct children will be
        -- inserted twice, which is absolutly fine and doesn't change correctness.
        let nameDirectChildrenAndProcess : NameContext -> Int -> [ObjectTree] -> WorkRes =
            lam ctx. lam nextId. lam children.
            match foldl (
                lam acc. lam child.
                let child = objTreeRemoveChildren child in
                match work child acc.ctx acc.nextId with
                { ctx = ctx, nextId = nextId } in -- We throw away the resulting direct child, but keep it in the nameMap
                { ctx = ctx, nextId = nextId }
            ) { ctx = ctx, nextId = nextId } children 
            with { ctx = ctx, nextId = nextId } in
            annotateAndProcess ctx nextId children
        in

        switch kind
        case ObjUse {} then
             let used = objName obj in
             match langNamespaceGetByName ctx.langNamespaceSet used with Some langNamespace then
                 let useThis : NameMap -> Int -> String -> [String] -> { nameMap: NameMap, nextId: Int } =
                     lam nameMap. lam nextId. lam kind. lam names.
                     foldl (
                         lam acc. lam name.
                         let namespace = join [objNamespace obj, "/", kind, "-", name] in
                         let url = buildUrl langNamespace.objIsStdlib namespace in
                         let entry = { entry = url, id = acc.nextId, namespace = namespace, isNested = true } in

                         log (join
                             ["Adding ", name, " in the name map from the usage of ", used, ".\n",
                             "namespace=", entry.namespace, "\n",
                             "url=", entry.entry, "\n",
                             "id=", int2string entry.id, "\n",
                             "isNested=", bool2string entry.isNested, "\n"]);

                         
                         let nameMap = nameMapInsert acc.nameMap name entry in
                         { nameMap = nameMap, nextId = addi acc.nextId 1 }
                     ) { nameMap = nameMap, nextId = nextId } names
                 in
                 let nameMap = ctx.nameMap in
                 match useThis nameMap nextId "syn" langNamespace.syns with { nameMap = nameMap, nextId = nextId } in
                 match useThis nameMap nextId "sem" langNamespace.sems with { nameMap = nameMap, nextId = nextId } in
                 match useThis nameMap nextId "con" langNamespace.cons with { nameMap = nameMap, nextId = nextId } in
                 match useThis nameMap nextId "type" langNamespace.types with { nameMap = nameMap, nextId = nextId } in

                 let ctx = { ctx with nameMap = nameMap } in
                 annotateAndProcess ctx nextId children
             else
                 namingWarn (join ["Failed to fetch the ", used, "lang."]);
                 { ctx = ctx, nextId = nextId, objTree = objTree}
        case ObjLang { parents = parents} then
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
           let langNamespaceSet = langNamespaceSetInsert ctx.langNamespaceSet (objName obj) langNamespace in
           
           let ctx = { ctx with langNamespaceSet = langNamespaceSet } in
          
           nameDirectChildrenAndProcess ctx nextId children
           
        case ObjRecursiveBloc {} then nameDirectChildrenAndProcess ctx nextId children
        case ObjInclude {} | ObjProgram {} then processAndAnnotate ctx nextId children
        case _ then annotateAndProcess ctx nextId children
        end
    in

    match work objTree (nameContextEmpty ()) 1 with { ctx = nameContext, objTree = annotatedObjTree } in
    { annotatedObjTree = annotatedObjTree, nameContext = nameContext }
