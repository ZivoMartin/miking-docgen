include "../../extracting/objects.mc"
include "../rendering-options.mc"
    
lang ObjectsRenderer = ObjectKinds + Formats
    
    sem objLangLink : String -> RenderingOptions -> String
    sem objLangLink =
    | name -> lam opt. join ["/Lang/lang-", name, ".lang.", formatGetExtention opt.fmt]
    
    -- Get URL link for an object
    sem objLink : Object -> RenderingOptions -> String
    sem objLink =
    | obj -> lam opt.
        let name = objName obj in
        let kind = objKind obj in
        let namespace = objNamespace obj in
        let ext = concat "." (formatGetExtention opt.fmt) in
        let link = switch kind
            case ObjLang {} | ObjUse {} then
                objLangLink name opt
            case ObjSem { langName = langName } | ObjSyn { langName = langName } then
                join ["/Lang/lang-", langName, "/", getFirstWord kind, "-", name, ext]        
            case _ then
                let prefix = if objIsStdlib obj then "Lib" else "Files" in
                join [prefix, namespace, ext]
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
