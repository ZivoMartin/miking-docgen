-- # ObjectKinds and Object helpers
--
-- This module defines:
-- - ObjectKind: describes different kinds of program elements (let, type, lang, sem, etc.)
-- - Object: carries name, namespace, doc and kind
-- - A simple ObjectTree type for grouping objects
--
-- Used in doc generation and object representation.

include "../util.mc"
    
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
    | ObjInclude { isStdlib: Bool }

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
    | ObjProgram {} -> ""
  
end

-- Object structure
type Object = use ObjectKinds in { name: String, doc : String, namespace: String, kind: ObjectKind }

-- Prefix length to truncate stdlib paths
let toTruncate = addi 1 (length stdlibLoc)

-- Get display title for an object
let objTitle : Object -> String = use ObjectKinds in lam obj.
    switch obj
    case { namespace = namespace, kind = ObjInclude { isStdlib = true } | ObjProgram { isStdlib = true } } then
        strTruncate namespace toTruncate
    case _ then obj.name
    end

-- Build lang link prefix
let getLangLink = lam name. concat "Lang/" name

-- Get URL link for an object
let objLink : Object -> String = use ObjectKinds in lam obj.
    switch obj
    case { name = name, kind = (ObjLang {} | ObjUse {}) } then concat (getLangLink name) ".lang"
    case { namespace = namespace, kind = ObjInclude { isStdlib = false } | ObjProgram { isStdlib = false } } then concatAll ["File/", namespace]
    case { namespace = namespace, kind = ObjInclude { isStdlib = true } | ObjProgram { isStdlib = true } } then concatAll ["Lib/", strTruncate namespace toTruncate]
    case { name = name, kind = (ObjSem { langName = langName } | ObjSyn { langName = langName }) & kind } then
        concatAll [getLangLink langName, "/", getFirstWord kind, "/", name]    
    case { name = name, namespace = namespace, kind = kind } then concatAll [getFirstWord kind, "/", namespace, "/", name, ".", getFirstWord kind]
    end

-- Get full namespace of object
let objNamespace : Object -> String = use ObjectKinds in lam obj.
    switch obj
    case { kind = (ObjProgram {} | ObjInclude {}), name = name } then name
    case { name = name, namespace = namespace } then concatAll [namespace, "/", name]
    end

-- Markdown formatted display for an object
let objMdFormat : Object -> String = use ObjectKinds in lam obj.
    let s = switch obj.kind
    case ObjLet { rec = rec, args = args } then
        let rec = if rec then "recursive " else "" in
        concatAll [rec, "let ", obj.name, " ", strJoin " " args]
    case ObjType { t = t } then concatAll ["type ", obj.name, match t with Some t then concat " : " t else ""]
    case ObjCon { t = t } then concatAll ["con ", obj.name, " : ", t]
    case ObjMexpr {} then "mexpr"
    case ObjProgram {} then ""
    case _ then concatAll [getFirstWord obj.kind, " ", obj.name] 
    end in
    match s with "" then "" else concatAll ["\n\n```\n", s, "\n```\n\n"]

-- Markdown specific doc:
-- - Lang shows parents
-- - Sem / Syn shows language + variants
-- - Let shows args
let objGetSpecificDoc : Object -> String = use ObjectKinds in lam obj.      
    switch obj
    case { kind = ObjLang { parents = parents & ([_] ++ _) } } then
        let parents = map (lam p. concatAll ["[", p, "](/", getLangLink p, ".lang)"]) parents in
        concatAll ["**Stem from:**\n\n ", (strJoin " + " parents), objMdFormat obj]
    case { name = name, kind = ( ObjSyn { langName = langName, variants = variants } | ObjSem { langName = langName, variants = variants } ) & kind } then
        let variants = concatAll (map (lam v. concatAll ["| ", v, "\n"]) variants) in
        concatAll [
            "From ", "[", langName, "](/", getLangLink langName, ".lang)\n\n",
            "```\n", getFirstWord kind, " ", name, "\n", variants, "```\n\n"
         ]
    case _ then objMdFormat obj
    end

-- Empty default object
let defaultObject : Object = use ObjectKinds in { name = "", doc = "", namespace = "", kind = ObjProgram { isStdlib = false } }

-- Object tree (hierarchy)
type ObjectTree
con ObjectNode : { obj: Object, sons: [ObjectTree] } -> ObjectTree
con ObjectLeaf : String -> ObjectTree
