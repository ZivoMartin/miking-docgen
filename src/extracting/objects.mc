-- # ObjectKinds and Object helpers
--
-- This module defines:
-- - ObjectKind: describes different kinds of program elements (let, type, lang, sem, etc.)
-- - Object: carries name, namespace, doc and kind
-- - A simple ObjectTree type for grouping objects
--
-- Used in doc generation and object representation.

include "../util.mc"
include "./source-code-builder.mc"
include "util.mc"
    
lang ObjectKinds

    -- All possible object kinds
    syn ObjectKind = 
    | ObjProgram { isStdlib: Bool }
    | ObjLet { rec : Bool, args : [String] }
    | ObjLang { parents : [String] }
    | ObjType { t: Option String }
    | ObjUse {}
    | ObjSem { langName: String, variants: [String] }
    | ObjSyn { langName: String, variants: [String] }
    | ObjCon { t: String }
    | ObjMexpr {}
    | ObjUtest {}    
    | ObjInclude { isStdlib: Bool, pathInFile: String }

    -- First keyword associated to this object kind (for printing / links)
    sem getFirstWord =
    | ObjLet {} -> "let"
    | ObjLang {} -> "lang"
    | ObjType {} -> "type"
    | ObjUse {} -> "use"
    | ObjSem {} -> "sem"
    | ObjSyn {} -> "syn"
    | ObjCon {} -> "con"
    | ObjMexpr {} -> "mexpr"
    | ObjInclude {} -> "include"
    | ObjUtest {} -> "utest"    
    | ObjProgram {} -> ""
  
end

-- Object structure
type Object = use ObjectKinds in { name: String, doc : String, namespace: String, kind: ObjectKind, sourceCode: SourceCode }

-- The position of where the program started
let basePosition : String = concat (sysGetCwd ()) "/"

-- Prefix length to truncate stdlib paths
let toTruncate = addi 1 (length stdlibLoc)

-- Get display title for an object
let objTitle : Object -> String = use ObjectKinds in lam obj.
    switch obj
    case { namespace = namespace, kind = ObjProgram { isStdlib = true } } then strTruncate namespace toTruncate
    case { kind = ObjInclude { pathInFile = pathInFile } } then pathInFile
    case { kind = ObjUtest {} } then "utest"
    case _ then obj.name
    end

-- Build lang link prefix
let getLangLink = lam name. concat "Lang/" name


-- Returns the absolute path of the object
let objAbsolutePath : Object -> String = lam obj. normalizePath (concat basePosition obj.namespace)
    
-- Get URL link for an object
let objLink : Object -> String = use ObjectKinds in lam obj.
    switch obj
    case { name = name, kind = (ObjLang {} | ObjUse {}) } then concat (getLangLink name) ".lang"
    case { namespace = namespace, kind = ObjInclude { isStdlib = false } | ObjProgram { isStdlib = false } } then concatAll ["File", objAbsolutePath obj]
    case { namespace = namespace, kind = ObjInclude { isStdlib = true } | ObjProgram { isStdlib = true } } then concatAll ["Lib", namespace]
    case { name = name, kind = (ObjSem { langName = langName } | ObjSyn { langName = langName }) & kind } then
        concatAll [getLangLink langName, "/", getFirstWord kind, "/", name]    
    case { name = name, namespace = namespace, kind = kind } then concatAll [getFirstWord kind, objAbsolutePath obj, "/", name, ".", getFirstWord kind]
    end
    
-- Get full namespace of object
let objNamespace : Object -> String = use ObjectKinds in lam obj.
    switch obj
    case { kind = (ObjProgram {} | ObjInclude {}), name = name } then name
    case { name = name, namespace = namespace } then concatAll [namespace, "/", name]
    end

-- Empty default object
let defaultObject : Object = use ObjectKinds in { name = "", doc = "", namespace = "", kind = ObjProgram { isStdlib = false }, sourceCode = sourceCodeEmpty () }
    
-- Returns a string representation of the object
let objToString = use ObjectKinds in lam kind. lam name.
    switch kind
    case ObjLet { rec = rec, args = args } then concatAll [if rec then "recursive " else "", "let ", name, " ", strJoin " " args]
    case ObjType { t = t } then concatAll ["type ", name, match t with Some t then concat " : " t else ""]
    case ObjCon { t = t } then concatAll ["con ", name, " : ", t]
    case ObjMexpr {} then "mexpr"
    case ObjProgram {} then ""
    case kind then concatAll [getFirstWord kind, " ", name]
    end
    
-- Object tree (hierarchy)
type ObjectTree
con ObjectNode : { obj: Object, sons: [ObjectTree] } -> ObjectTree
con ObjectLeaf : String -> ObjectTree

-- Extract all the `ObjectNode`s in an array of ObjectTree
let objectSonsFilterNodes : [ObjectTree] -> [ObjectTree] = lam sons. filter (lam s. match s with ObjectNode {} then true else false) sons
