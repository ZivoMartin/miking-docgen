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

    let buildUrl = buildUrl opt.stdlibFolder opt.urlPrefix opt.fmt in

    type WorkRes = { ctx: NameContext, nextId: Int, objTree: ObjectTree } in
    recursive let work : ObjectTree -> NameContext -> Int -> WorkRes = use ObjectForms in
        lam objTree. lam ctx. lam nextId. 


        let obj = objTreeObj objTree in
        let children = objTreeChildren objTree in
        let kind = objForm obj in

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
        
        let annotate : NameContext -> ObjectTree -> Int -> WorkRes =
            lam ctx. lam objTree. lam nextId.

            let children = objTreeChildren objTree in
            let obj = objTreeObj objTree in
            let obj = objWithId obj nextId in
            let nextId = addi 1 nextId in
            let objTree = ObjectNode { obj = obj, children = children } in

            let ctx = 
                if objFormHasUrl kind then
                    let name = objName obj in
                    let namespace = objNamespace obj in
                    let isStdlib = objIsStdlib obj in

                    let url = buildUrl isStdlib namespace in
                    let value = { url = url, obj = objWithSourceCode obj (sourceCodeEmpty ()) } in
                    let entry = { entry = value, id = objId obj, namespace = namespace, isNested = isNested } in

                    log (join
                        ["Adding ", name, " in the name map.\n",
                        "namespace=", entry.namespace, "\n",
                        "url=", entry.entry.url, "\n",
                        "id=", int2string entry.id, "\n",
                        "isNested=", bool2string entry.isNested, "\n"]);

                    let nameMap = nameMapInsert ctx.nameMap name namespace entry in
                    { ctx with nameMap = nameMap }
                else ctx
            in            
            { ctx = ctx, nextId = nextId, objTree = objTree }
        in

        let annotateAndProcess : ObjectTree -> NameContext -> Int -> [ObjectTree] -> WorkRes =
            lam objTree. lam ctx. lam nextId. lam children.
            match annotate ctx objTree nextId with
            { ctx = ctx, nextId = nextId, objTree = objTree } in
            process ctx objTree nextId children
        in

        let processAndAnnotate : ObjectTree -> NameContext -> Int -> [ObjectTree] -> WorkRes =
            lam objTree. lam ctx. lam nextId. lam children.
            match process ctx objTree nextId children with
            { ctx = ctx, nextId = nextId, objTree = objTree } in
            annotate ctx objTree nextId
        in

        -- We first insert the direct children, then we call process. So direct children will be
        -- inserted twice, which is absolutly fine and doesn t change correctness.
        let nameDirectChildrenAndProcess : ObjectTree -> NameContext -> Int -> WorkRes =
            lam objTree. lam ctx. lam nextId.
            let children = objTreeChildren objTree in
            match foldl (
                lam acc. lam child.
                let child = objTreeRemoveChildren child in
                match work child acc.ctx acc.nextId with
                { ctx = ctx, nextId = nextId } in -- We throw away the resulting direct child, but keep it in the nameMap
                { ctx = ctx, nextId = nextId }
            ) { ctx = ctx, nextId = nextId } children 
            with { ctx = ctx, nextId = nextId } in
            annotateAndProcess objTree ctx nextId children
        in

        switch kind
        case ObjUse {} then
             let used = objName obj in
             match namespaceSetGetByName ctx.langNamespaceSet used with Some langNamespace then
                 let langNamespace = langNamespace.full in

                 let useThis : NameMap -> Int -> String -> [Object] -> { nameMap: NameMap, nextId: Int } =
                     lam nameMap. lam nextId. lam kind. lam objects.
                     foldl (
                         lam acc. lam obj.

                         let name = objName obj in
                         let namespace = join [objNamespace obj, "/", kind, "-", name] in
                         let url = buildUrl langNamespace.objIsStdlib namespace in
                         let value = { url = url, obj = objWithSourceCode obj (sourceCodeEmpty ()) } in

                         let entry = { entry = value, id = acc.nextId, namespace = namespace, isNested = true } in

                         log (join
                             ["Adding ", name, " in the name map from the usage of ", used, ".\n",
                             "namespace=", entry.namespace, "\n",
                             "url=", entry.entry.url, "\n",
                             "id=", int2string entry.id, "\n",
                             "isNested=", bool2string entry.isNested, "\n"]);

                         let nameMap = nameMapInsert acc.nameMap name namespace entry in
                         { nameMap = nameMap, nextId = addi acc.nextId 1 }
                     ) { nameMap = nameMap, nextId = nextId } objects
                 in
                 let nameMap = ctx.nameMap in
                 match useThis nameMap nextId "syn" langNamespace.syns with { nameMap = nameMap, nextId = nextId } in
                 match useThis nameMap nextId "sem" langNamespace.sems with { nameMap = nameMap, nextId = nextId } in
                 match useThis nameMap nextId "con" langNamespace.cons with { nameMap = nameMap, nextId = nextId } in
                 match useThis nameMap nextId "type" langNamespace.types with { nameMap = nameMap, nextId = nextId } in

                 let ctx = { ctx with nameMap = nameMap } in
                 annotateAndProcess objTree ctx nextId children
             else
                 namingWarn (join ["Failed to fetch the ", used, "lang."]);
                 { ctx = ctx, nextId = nextId, objTree = objTree}
        case ObjLang { parents = parents} then
            let filterIt : (ObjectForm -> Bool) -> [Object] =
                lam keepIt.
                mapOption (
                    lam child.
                    let obj = objTreeObj child in
                    if keepIt (objForm obj) then
                       Some (langNamespaceCleanObj obj)
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

           let name = objName obj in
           let namespace = objNamespace obj in
           let isStdlib = objIsStdlib obj in

           let langNamespace = langNamespaceSetBuildNamespace ctx.langNamespaceSet langNamespace parents in
           let langNamespaceSet = langNamespaceSetInsert ctx.langNamespaceSet name langNamespace in
           let ctx = { ctx with langNamespaceSet = langNamespaceSet } in

           let updateChildren : [ObjectTree] -> (Object -> [ObjectTree]) -> LangNamespace -> [ObjectTree] =
               lam children. lam cast. lam namespace.

               let createChildren : [Object] -> [ObjectTree] =
                   lam updatedChildren: [Object].
                   join (map cast updatedChildren)
               in

               let syns = createChildren namespace.syns in
               let sems = createChildren namespace.sems in
               let cons = createChildren namespace.cons in
               let types = createChildren namespace.types in

               join [children, syns, sems, cons, types]
           in


           let explicit = match langNamespaceGetExplicitChildren langNamespaceSet name with Some explicit then explicit else
                       namingWarn (join ["Failed to fetch the explicit lang namespace of ", name, "."]); langNamespaceDefault
           in
           
           let implicit = match langNamespaceGetImplicitChildren langNamespaceSet name with Some implicit then implicit else
                       namingWarn (join ["Failed to fetch the implicit lang namespace of ", name, "."]); langNamespaceDefault
           in

           let children = updateChildren [] (
                   lam obj.
                   let filtered = filter (lam original. eqString (objTreeName original) (objName obj)) children in
                   if null filtered then
                        namingWarn (join ["Explicit children of the lang namespace and actual children doesn't match for ", objName obj, "."]);
                        [ObjectNode { children = [], obj = obj }]                        
                   else
                        let filtered = reverse filtered in
                        
                        let last = head filtered in
                        let sourceCode = objSourceCode (objTreeObj last) in
                        let lastObj = objWithSourceCode obj sourceCode in
                        let lastChildren = objTreeChildren last in
                        let last = ObjectNode { obj = lastObj, children = lastChildren } in
                        let filtered = cons last (tail filtered) in
                        reverse filtered
              ) explicit
           in

           let children = updateChildren children (lam obj. [ObjectNode { children = [], obj = obj }] ) implicit in

           let objTree = ObjectNode { children = children, obj = objTreeObj objTree } in

           nameDirectChildrenAndProcess objTree ctx nextId
           
        case ObjRecursiveBloc {} then nameDirectChildrenAndProcess objTree ctx nextId
        case ObjCon {} then
            let obj = objWithId obj nextId in
            let ctx = { ctx with typeNamespaceSet = typeNamespaceInsertNewCon ctx.typeNamespaceSet obj } in
            annotateAndProcess objTree ctx nextId children
        case ObjType {} then
            let obj = objWithId obj nextId in        
            let ctx = { ctx with typeNamespaceSet = typeNamespaceInsertNewType ctx.typeNamespaceSet obj } in
            annotateAndProcess objTree ctx nextId children
        case ObjInclude {} | ObjProgram {} then processAndAnnotate objTree ctx nextId children
        case _ then
             if objRenderIt obj then annotateAndProcess objTree ctx nextId children
             else { ctx = ctx, nextId = nextId, objTree = objTree }
        end
    in

    match work objTree (nameContextEmpty ()) 1 with { ctx = nameContext, objTree = annotatedObjTree } in
    { annotatedObjTree = annotatedObjTree, nameContext = nameContext }
