include "../util.mc"
    
lang ObjectKinds

    syn ObjectKind = 
    | ObjProgram {}
    | ObjLet { rec : Bool, args : [String] }
    | ObjLang { parents : [String] }
    | ObjType { t: Option String }
    | ObjUse {}
    | ObjSem { langName: String }
    | ObjSyn { langName: String }
    | ObjCon { t: String }
    | ObjMexpr {}
    | ObjInclude {}

    sem getFirstWord =
    | ObjLet {} -> "let"
    | ObjLang {} -> "lang"
    | ObjType {} -> "type"
    | ObjUse {} -> "use"
    | ObjSem {} -> "sem"
    | ObjSyn {} -> "syn"
    | ObjCon {} -> "con"
    | ObjMexpr {} -> "mexpr"
    | ObjProgram {} -> ""
    | ObjInclude {} -> error "No first word for an include."
  
end

type Object = use ObjectKinds in { name: String, doc : String, namespace: String, kind: ObjectKind }

let objMdTitle : Object -> String = lam obj. obj.name

let getLangLink = lam name. concat "Lang-" name 
    
let objLink : Object -> String = use ObjectKinds in lam obj.
    switch obj
    case { name = name, kind = (ObjLang {} | ObjUse {}) } then getLangLink name
    case { name = name, kind = (ObjProgram {} | ObjInclude {}) } then concatAll ["File-", sanitizePath name]
    case { name = name, namespace = namespace, kind = kind } then concatAll [getFirstWord kind, "-", name, "-", (sanitizePath namespace)]
    end

let objNamespace : Object -> String = use ObjectKinds in lam obj.
    switch obj
    case { kind = ObjProgram {}, name = name } then name
    case { name = name, namespace = namespace } then concatAll [namespace, "-", name]
    end


    
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
    match s with "" then "" else concatAll ["\n\n```ocaml\n", s, "\n```\n\n"]

    
-- Returns the documentation for this object in an md format:
-- - Lang : Displays the parents
-- - Sem and Syn : Displays the language they are declared into
-- - Let : Displays the name of the function and his arguments.
let objGetSpecificDoc : Object -> String = use ObjectKinds in lam obj.
    concatAll [
        switch obj
        case { kind = ObjLang { parents = parents & ([_] ++ _) } } then
            let parents = map (lam p. concatAll ["[", p, "](", getLangLink p, ")"]) parents in
            concat "**Stem from:**\n\n " (strJoin " + " parents)
        case { kind = ( ObjSyn { langName = langName } | ObjSem { langName = langName } ) } then
            concatAll ["From ", "[", langName, "](", getLangLink langName, ")"]
        case { kind = ObjProgram {} } then ""
        case _ then ""
        end,
        "\n\n",
        objMdFormat obj,
        "\n\n"
    ]


    
let defaultObject : Object = use ObjectKinds in { name = "", doc = "", namespace = "", kind = ObjProgram {} }

type ObjectTree
con ObjectNode : { obj: Object, sons: [ObjectTree] } -> ObjectTree
con ObjectLeaf : String -> ObjectTree
