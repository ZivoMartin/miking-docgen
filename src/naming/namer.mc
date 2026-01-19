include "./langs-namespace.mc"
include "./name-map.mc"
include "./name-context.mc"
include "../extracting/objects.mc"

type NamingRes = use Objects in  {
     annotatedObj: Object,
     nameContext: NameContext
}

let name : use Objects in Logger -> NamingOptions -> Object -> NamingRes =
    lam log. lam opt. lam obj.
    use Objects in

    let buildUrl = buildUrl opt.stdlibFolder opt.urlPrefix opt.fmt in

    type WorkRes = { ctx: NameContext, nextId: Int, obj: Object } in

    recursive let work : Object -> NameContext -> Int -> WorkRes = use Objects in
        lam obj. lam ctx. lam nextId. 

        let originalChildren = objChildren obj in

        let process : NameContext -> Object -> Int -> [Object] -> WorkRes =
            lam ctx. lam obj. lam nextId. lam children.

            let res = foldl (
                lam acc. lam child.
                let res = work child acc.ctx acc.nextId in
                { res with obj = objAddChild acc.obj res.obj }
            ) { nextId = nextId, ctx = ctx, obj = objWithoutChildren obj } children
            in

            { res with obj = objReverseChildren res.obj }
        in
        
        let annotate : NameContext -> Object -> Int -> WorkRes =
            lam ctx. lam obj. lam nextId.

            let children = objChildren obj in
            let obj = objWithId obj nextId in
            let nextId = addi 1 nextId in

            let ctx = 
                if objHasLink obj then
                    let name = objName obj in
                    let namespace = objNamespace obj in
                    let isStdlib = objIsStdlib obj in

                    let url = buildUrl isStdlib namespace in
                    let value = { url = url, obj = objWithSourceCode obj (sourceCodeEmpty ()) } in
                    let entry = { entry = value, id = objId obj, namespace = namespace } in

                    let nameMap =
                        if objHasUrl obj then nameMapInsert ctx.nameMap name namespace entry
                        else ctx.nameMap
                    in

                    { ctx with nameMap = nameMap }
                else ctx
            in            
            { ctx = ctx, nextId = nextId, obj = obj }
        in

        let annotateAndProcess : Object -> NameContext -> Int -> [Object] -> WorkRes =
            lam obj. lam ctx. lam nextId. lam children.
            match annotate ctx obj nextId with
            { ctx = ctx, nextId = nextId, obj = obj } in
            process ctx obj nextId children
        in

        let processAndAnnotate : Object -> NameContext -> Int -> [Object] -> WorkRes =
            lam obj. lam ctx. lam nextId. lam children.
            match process ctx obj nextId children with
            { ctx = ctx, nextId = nextId, obj = obj } in
            annotate ctx obj nextId
        in

        -- We first insert the direct children, then we call process. So direct children will be
        -- inserted twice, which is absolutly fine and doesn't change correctness.
        let nameDirectChildrenAndProcess : Object -> NameContext -> Int -> WorkRes =
            lam obj. lam ctx. lam nextId.
            let children = objChildren obj in
            match foldl (
                lam acc. lam child.
                let child = objWithoutChildren child in
                match work child acc.ctx acc.nextId with
                { ctx = ctx, nextId = nextId } in -- We throw away the resulting direct child, but keep it in the nameMap
                { ctx = ctx, nextId = nextId }
            ) { ctx = ctx, nextId = nextId } children 
            with { ctx = ctx, nextId = nextId } in
            annotateAndProcess obj ctx nextId children
        in

        switch obj
        case ObjLang { parents = parents} then
            let filterIt : (Object -> Bool) -> [Object] =
                lam keepIt.
                mapOption (
                    lam child.
                    if keepIt child then
                       Some (langNamespaceCleanObj child)
                    else
                       None {}
                ) originalChildren
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

           let updateChildren : [Object] -> (Object -> [Object]) -> LangNamespace -> [Object] =
               lam children. lam cast. lam namespace.

               let createChildren : [Object] -> [Object] =
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

           -- TODO: Check if we are not injecting n square children here.
           let children = updateChildren [] (
                   lam obj.
                   let filtered = filter (lam original. eqString (objName original) (objName obj)) originalChildren in
                   if null filtered then
                        namingWarn (join ["Explicit children of the lang namespace and actual children doesn't match for ", objName obj, "."]);
                        [objWithoutChildren obj]                        
                   else
                        let filtered = reverse filtered in
                        
                        let last = head filtered in
                        let lastSourceCode = objSourceCode last in
                        let lastChildren = objChildren last in

                        let last = objWithSourceCode obj lastSourceCode in
                        let last = objSetChildren last lastChildren in

                        let filtered = cons last (tail filtered) in
                        reverse filtered
              ) explicit
           in

           -- Not really necessary by the way
           let children = updateChildren children (lam obj. [objWithoutChildren obj] ) implicit in

           let obj = objSetChildren obj children in

           nameDirectChildrenAndProcess obj ctx nextId
           
        case ObjCon {} then
            let namedObj = objWithId obj nextId in
            let ctx = { ctx with typeNamespaceSet = typeNamespaceInsertNewCon ctx.typeNamespaceSet namedObj } in
            annotateAndProcess obj ctx nextId originalChildren
        case ObjType {} then
            let namedObj = objWithId obj nextId in        
            let ctx = { ctx with typeNamespaceSet = typeNamespaceInsertNewType ctx.typeNamespaceSet namedObj } in
            annotateAndProcess obj ctx nextId originalChildren
        case ObjInclude {} | ObjProgram {} then processAndAnnotate obj ctx nextId originalChildren
        case _ then annotateAndProcess obj ctx nextId originalChildren
        end
    in

    match work obj (nameContextEmpty ()) 1 with { ctx = nameContext, obj = annotatedObj } in
    { annotatedObj = annotatedObj, nameContext = nameContext }
