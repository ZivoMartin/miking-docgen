include "../../extracting/objects.mc"
include "../rendering-options.mc"
    
lang ObjectsRenderer = ObjectKinds + Formats

    -- Get URL link for an object
    sem objLink : Object -> RenderingOptions -> String
    sem objLink =
    | obj -> lam opt.
        let name = objName obj in
        let kind = objKind obj in
        let namespace = objNamespace obj in
        let link = switch kind
            case ObjLang {} | ObjUse {} then
                concat (getLangLink name) ".lang"
            case ObjSem { langName = langName } | ObjSyn { langName = langName } then
                join [getLangLink langName, "/", getFirstWord kind, "/", name]        
            case ObjInclude { isStdlib = true } | ObjProgram { isStdlib = true } then
                concat "Lib" namespace
            case _ then
                concat "File" namespace
            end
        in
        let link = if strStartsWith "/" link then link else cons '/' link in
        join [link, ".", formatGetExtention opt.fmt]
            
    -- Get display title for an object
    sem objTitle : Object -> String
    sem objTitle =    
    | obj ->
        let name = objName obj in
        let kind = objKind obj in
        let namespace = objNamespace obj in
        switch kind
        case ObjProgram { isStdlib = true } then strTruncate namespace (addi 1 (length stdlibLoc))
        case ObjInclude { pathInFile = pathInFile } then pathInFile
        case ObjUtest {} then "utest"
        case _ then name
        end
end
