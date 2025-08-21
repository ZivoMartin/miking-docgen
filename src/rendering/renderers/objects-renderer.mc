include "../../extracting/objects.mc"
include "../rendering-options.mc"

lang ObjectsRenderer = ObjectKinds + Formats
    
    sem objLangLink : String -> RenderingOptions -> String
    sem objLangLink =
    | name -> lam opt. match hmLookup name opt.nameContext with Some link then link
                       else name
    
    sem objNameIfHas : Object -> Option String
    sem objNameIfHas =
    | { kind = ObjLet {} | ObjType {} | ObjSem {} | ObjSyn {} | ObjLang {} | ObjCon {} } & obj -> Some (objName obj)
    | _ -> None {}


    sem objPreserveNameCtx : Object -> Bool
    sem objPreserveNameCtx =
    | { kind = ObjLang {} | ObjProgram {} } -> true
    | _ -> false

    sem objGetPureLink : Object -> RenderingOptions -> String
    sem objGetPureLink =
    | obj -> lam opt.
        let namespace = objNamespace obj in
        let ext = concat "." (formatGetExtention opt.fmt) in
        let prefix = if objIsStdlib obj then "Lib" else "Files" in
        let link =  join [prefix, namespace, ext] in
        if strStartsWith "/" link then link else cons '/' link     

    -- Get URL link for an object
    sem objLink : Object -> RenderingOptions -> String
    sem objLink =
    | obj -> lam opt.
        let link = switch (objKind obj)
            case ObjLang {} | ObjUse {} then
                objLangLink (objName obj) opt   
            case _ then
                 objGetPureLink obj opt
            end
        in
        if strStartsWith "/" link then link else cons '/' link
            
    -- Get display title for an object
    sem objTitle : Object -> String
    sem objTitle =    
    | obj ->
        let name = head (reverse (strSplit "/" (objName obj))) in
        let kind = objKind obj in
        switch kind
        case ObjInclude { pathInFile = pathInFile } then pathInFile
        case ObjUtest {} then "utest"
        case _ then name
        end


    sem objLog : Object -> RenderingOptions -> ()
    sem objLog =
    | obj -> lam opt. renderingLog (join [
        "Object ", objName obj, ":\n",
        "   kind: ", objKindToString (objKind obj), "\n",
        "   namespace: ", objNamespace obj, "\n",
        "   prefix: ", objPrefix obj, "\n",
        "   link: ", objLink obj opt, "\n",
        "   isStdlib: ", bool2string (objIsStdlib obj), "\n"
    ])
end
