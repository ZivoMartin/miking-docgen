-- # ObjectKinds and Object helpers
--
-- This module defines:
-- - ObjectKind: describes different kinds of program elements (let, type, lang, sem, etc.)
-- - Object: carries name, namespace, doc and kind
-- - A simple ObjectTree type for grouping objects
--
-- Used in doc generation and object representation.

include "../global/util.mc"
include "./source-code-builder.mc"
include "util.mc"

include "mexpr/ast.mc"
    
lang ObjectKinds = MExprAst

    -- All possible object kinds
    syn ObjectKind = 
    | ObjProgram { isStdlib: Bool }
    | ObjLet { rec : Bool, args : [String], ty: Option Type }
    | ObjLang { parents : [String] }
    | ObjType { t: Option String }
    | ObjUse {}
    | ObjSem { langName: String, variants: [String], ty: Option Type }
    | ObjSyn { langName: String, variants: [String] }
    | ObjCon { t: String }
    | ObjMexpr {}
    | ObjUtest {}
    | ObjRecursiveBlock {}
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
    | ObjRecursiveBlock {} -> "recursive"
    | ObjProgram {} -> ""
    | _ -> warn "All object kinds are not supported in getFirstWord sementic"; ""
  
end

-- Object structure
type Object = use ObjectKinds in { name: String, doc : String, namespace: String, kind: ObjectKind, sourceCode: SourceCode, prefix: String }

-- The position of where the program started
let basePosition : String = concat (sysGetCwd ()) "/"
    
-- Build lang link prefix
let getLangLink = lam name. concat "/Lang/" name

let objName : Object -> String = lam obj. obj.name
let objKind : Object -> use ObjectKinds in ObjectKind = lam obj. obj.kind
let objDoc : Object -> String = lam obj. obj.doc
let objSourceCode : Object -> SourceCode = lam obj. obj.sourceCode    
let objNamespace : Object -> String = use ObjectKinds in lam obj. obj.namespace
let objPrefix : Object -> String = lam obj. obj.prefix

let objWithName : Object -> String -> Object = lam obj. lam name. { obj with name = name }
let objWithKind : Object -> use ObjectKinds in ObjectKind -> Object = lam obj. lam kind. { obj with kind = kind }
let objWithDoc : Object -> String -> Object = lam obj. lam doc. { obj with doc = doc }
let objWithSourceCode : Object -> SourceCode -> Object = lam obj. lam sourceCode. { obj with sourceCode = sourceCode }

let objWithPrefix: Object -> String -> Object = lam obj. lam prefix.
    let process = lam.
        let basePrefix = normalizePath (concat basePosition obj.namespace) in
        let lengthBasePrefix = length basePrefix in
        let lengthPrefix = length prefix in
        if gti lengthPrefix lengthBasePrefix then
             extractingWarn "The prefix is longer than one of the object's namespace";
             basePrefix
        else subsequence basePrefix lengthPrefix lengthBasePrefix
    in
    let namespace = match prefix with "" then obj.namespace else process () in
    let namespace = if strStartsWith "/" namespace then namespace else cons '/' namespace in
    { obj with namespace = namespace, prefix = prefix }
    
let objWithNamespace : Object -> String -> Object = lam obj. lam namespace.
    let namespace =
    if strStartsWith stdlibLoc namespace then
        subsequence namespace (length stdlibLoc) (length namespace)
    else namespace in
    let obj = { obj with namespace = namespace } in
    objWithPrefix obj obj.prefix

let objAbsolutePath : Object -> String = lam obj. concat obj.prefix obj.namespace

-- Empty default object
let defaultObject : Object = use ObjectKinds in { name = "", doc = "", namespace = "", kind = ObjProgram { isStdlib = false }, sourceCode = sourceCodeEmpty (), prefix = "" }

let objGetLangName : Object -> String = use ObjectKinds in lam obj.
    match obj.kind with ObjSem { langName = langName } | ObjSyn { langName = langName } then langName else ""


-- Returns a string representation of the object
let objToString = use ObjectKinds in lam kind. lam name.
    switch kind
    case ObjLet { rec = rec, args = args } then join [if rec then "recursive " else "", "let ", name, " ", strJoin " " args]
    case ObjType { t = t } then join ["type ", name, match t with Some t then concat " : " t else ""]
    case ObjCon { t = t } then join ["con ", name, " : ", t]
    case ObjMexpr {} then "mexpr"
    case ObjProgram {} then ""
    case kind then join [getFirstWord kind, " ", name]
    end

let objSetType = use ObjectKinds in lam obj. lam ty.
    { obj with kind = switch obj.kind
    case ObjLet d then ObjLet { d with ty = ty }
    case ObjSem d then ObjSem { d with ty = ty }    
    case _ then obj.kind end }
    
    
-- Object tree (hierarchy)
type ObjectTree
con ObjectNode : { obj: Object, sons: [ObjectTree] } -> ObjectTree

let objTreeToString = lam obj. match obj with ObjectNode { obj = obj } in objToString obj.kind obj.name
