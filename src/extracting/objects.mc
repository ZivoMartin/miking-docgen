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
    | ObjProgram {}
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
    | ObjInclude { pathInFile: String }

    sem objKindToString : ObjectKind -> String
    sem objKindToString =
    | ObjLet { rec = rec, args = args, ty = ty} -> join ["ObjLet, recursive: ", bool2string rec, ", args: [", strJoin ", " args, "]"]
    | ObjLang { parents = parents } -> join ["ObjLang, parents: ", strJoin ", " parents]
    | ObjType { t = t } -> join ["ObjType", match t with Some t then concat ", " t else ""]
    | ObjUse {} -> "ObjUse"
    | ObjSem { langName =  langName } ->  join ["ObjSem, langName = ", langName]
    | ObjSyn { langName = langName } -> join ["ObjSyn, langName = ", langName]
    | ObjCon { t = t } -> join ["ObjCon: ", t]
    | ObjMexpr {} -> "ObjMexpr"
    | ObjInclude { pathInFile = p } -> join ["ObjInclude, path = ", p]
    | ObjUtest {} -> "ObjUtest"
    | ObjRecursiveBlock {} -> "ObjRecursiveBlock"
    | ObjProgram {} -> "ObjProgram"
    | _ -> warn "All object kinds are not supported in objKindToString sementic"; ""
         
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
type Object = use ObjectKinds in { name: String, doc : String, namespace: String, kind: ObjectKind, sourceCode: SourceCode, prefix: String, isStdlib: Bool }

-- The position of where the program started
let basePosition : String = concat (sysGetCwd ()) "/"

let objName : Object -> String = lam obj. obj.name
let objKind : Object -> use ObjectKinds in ObjectKind = lam obj. obj.kind
let objDoc : Object -> String = lam obj. obj.doc
let objSourceCode : Object -> SourceCode = lam obj. obj.sourceCode    
let objNamespace : Object -> String = use ObjectKinds in lam obj. obj.namespace
let objPrefix : Object -> String = lam obj. obj.prefix
let objIsStdlib : Object -> Bool = lam obj. obj.isStdlib

let objWithName : Object -> String -> Object = lam obj. lam name. { obj with name = name }
let objWithKind : Object -> use ObjectKinds in ObjectKind -> Object = lam obj. lam kind. { obj with kind = kind }
let objWithDoc : Object -> String -> Object = lam obj. lam doc. { obj with doc = doc }
let objWithIsStdlib : Object -> Bool -> Object = lam obj. lam isStdlib. { obj with isStdlib = isStdlib }    
let objWithSourceCode : Object -> SourceCode -> Object = lam obj. lam sourceCode. { obj with sourceCode = sourceCode }

let objWithPrefix: Object -> String -> Object = lam obj. lam prefix.
    let process = lam.
        let basePrefix: String = normalizePath (concat basePosition obj.namespace) in
        let lengthBasePrefix = length basePrefix in
        let lengthPrefix = length prefix in
        if strStartsWith prefix basePrefix then subsequence basePrefix lengthPrefix lengthBasePrefix
        else
            extractingWarn (join ["The namespace ", basePrefix, "does not start with the prefix ", prefix, "."]);
            basePrefix
    in
    let namespace = match prefix with "" then obj.namespace else process () in
    let namespace = if strStartsWith "/" namespace then namespace else cons '/' namespace in
    { obj with namespace = namespace, prefix = prefix }
    
let objWithNamespace : Object -> String -> Object = lam obj. lam namespace.
    let namespace =
    if strStartsWith stdlibLoc namespace then
        subsequence namespace (length stdlibLoc) (length namespace)
    else
        namespace
    in
    let obj = { obj with namespace = namespace } in
    objWithPrefix obj obj.prefix

let objAbsolutePath : Object -> String = lam obj.
    concat obj.prefix obj.namespace

-- Empty default object
let defaultObject : Object = use ObjectKinds in { name = "", doc = "", namespace = "", isStdlib = false, kind = ObjProgram {}, sourceCode = sourceCodeEmpty (), prefix = "" }

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

let objTreeToString : ObjectTree -> String = lam tree. match tree with ObjectNode { obj = obj } in objToString obj.kind obj.name
let objTreeObj : ObjectTree -> Object = lam tree. match tree with ObjectNode { obj = obj } in obj
let objTreeDoc : ObjectTree -> String = lam tree. objDoc (objTreeObj tree)
let objTreeSourceCode : ObjectTree -> SourceCode = lam tree. objSourceCode (objTreeObj tree)
let objTreeWithDoc : ObjectTree -> String -> ObjectTree = lam tree. lam doc.
    match tree with ObjectNode { obj = obj, sons = sons } in ObjectNode { obj = { obj with doc = doc}, sons = sons }
let objTreeWithSourceCode : ObjectTree -> SourceCode -> ObjectTree = lam tree. lam code.
    match tree with ObjectNode { obj = obj, sons = sons } in ObjectNode { obj = { obj with sourceCode = code}, sons = sons }
