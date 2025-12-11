-- # Object definition  
--
-- This module defines:
-- - `Object`: carries name, namespace, doc, form, source code, prefix, stdlib flag
-- - `ObjectTree`: a simple tree wrapper for grouping objects
--
-- Used in doc generation and object representation.

include "../global/util.mc"
include "./source-code-builder.mc"
include "./object-form.mc"
include "./util.mc"

-- The object type is designed to represent the documentation-side structure of the code.
-- Its fields are:
-- - `name`: The name of the object. For a `let`, it corresponds to the variable name.  
-- - `doc`: All comments above the beginning of the block.  
-- - `namespace`: The namespace reflects the current position of the node in the tree and is used
--   to build its documentation path.  
-- - `form`: Specific to the object’s type (see ObjectForm).  
-- - `sourceCode`: An **absolute** representation of the object’s source code.
--   It is not just a plain string, but a structured value defined in `source-code-word.mc` and `source-code-builder.mc`.  
-- - `isStdlib`: Marks whether the object belongs to the stdlib.
-- - `renderIt` : Indicates if the object should be rendered during rendering stage.
type Object = use ObjectForms in {
    name: String,
    doc : String,
    namespace: String,
    form: ObjectForm,
    sourceCode: SourceCode,
    isStdlib: Bool,
    renderIt: Bool,
    id: Int
}

-- Absolute filesystem position of the current program start.
let basePosition : String = concat (sysGetCwd ()) "/"

-- Simple field accessors.
let objName : Object -> String = lam obj. obj.name
let objForm : Object -> use ObjectForms in ObjectForm = lam obj. obj.form
let objDoc : Object -> String = lam obj. obj.doc
let objSourceCode : Object -> SourceCode = lam obj. obj.sourceCode    
let objNamespace : Object -> String = use ObjectForms in lam obj. obj.namespace
let objIsStdlib : Object -> Bool = lam obj. obj.isStdlib
let objRenderIt : Object -> Bool = lam obj. obj.renderIt
let objId : Object -> Int = lam obj. obj.id

-- Object updaters (immutable setters).
let objWithName : Object -> String -> Object = lam obj. lam name. { obj with name = name }
let objWithForm : Object -> use ObjectForms in ObjectForm -> Object = lam obj. lam form. { obj with form = form }
let objWithDoc : Object -> String -> Object = lam obj. lam doc. { obj with doc = doc }
let objWithIsStdlib : Object -> Bool -> Object = lam obj. lam isStdlib. { obj with isStdlib = isStdlib }    
let objWithSourceCode : Object -> SourceCode -> Object = lam obj. lam sourceCode. { obj with sourceCode = sourceCode }
let objWithRenderIt : Object -> Bool -> Object = lam obj. lam renderIt. { obj with renderIt = renderIt }
let objWithId : Object -> Int -> Object = lam obj. lam id. { obj with id = id }

-- Sets a shorter namespace by removing `prefix`; stores the prefix for recovery.
-- Warns if the namespace does not start with the given prefix.
let objWithPrefix: Object -> String -> Object = lam obj. lam prefix.
    let process = lam.
        let basePrefix = obj.namespace in
        let lengthBasePrefix = length basePrefix in
        let lengthPrefix = length prefix in
        
        if objIsStdlib obj then basePrefix
        else if strStartsWith prefix basePrefix then
            subsequence basePrefix lengthPrefix lengthBasePrefix
        else
            error (join ["The namespace ", basePrefix, " does not start with the prefix ", prefix, "."]);
            basePrefix
    in
    let namespace = match prefix with "" then obj.namespace else process () in
    let namespace =
        if strStartsWith "/" namespace then namespace
        else cons '/' namespace
    in
    { obj with namespace = namespace }
    
-- Replaces namespace; strips stdlib prefix if present; re-applies stored `prefix`.
let objWithNamespace : Object -> String -> Object = lam obj. lam namespace.
    let namespace =
    if strStartsWith stdlibLoc namespace then
        subsequence namespace (length stdlibLoc) (length namespace)
    else
        namespace
    in

    { obj with namespace = namespace }

-- Returns true if the object has a meaningful id.
let objHasId : Object -> Bool = lam obj. neqi obj.id 0

-- Returns true if the object has a code source (otherwise it has probably been added during naming)
let objHasSourceCode : Object -> Bool = lam obj. not (null obj.sourceCode)

-- Returns absolute path = prefix + namespace.
let objAbsolutePath : Object -> String -> String =
    lam obj. lam prefix.
    concat prefix obj.namespace

let objDefaultDoc : String = "No documentation available here."

-- Empty default object (neutral values).
let defaultObject : Object = use ObjectForms in {
    name = "",
    doc = "",
    namespace = "",
    renderIt = false,
    isStdlib = false,
    form = ObjProgram {},
    sourceCode = sourceCodeEmpty (),
    id = 0
}

let objTryGetDoc : Object -> String = lam obj.
    let doc = objDoc obj in
    if eqString doc objDefaultDoc then "" else doc

-- Extracts the language name from a Sem/Syn object; else empty string.
let objGetLangName : Object -> String = use ObjectForms in lam obj.
    match obj.form with ObjSem { langName = langName } | ObjSyn { langName = langName } then langName else ""

-- Renders a short textual representation of an object (for printing).
let objToString = use ObjectForms in lam form. lam name.
    switch form
    case ObjLet { rec = rec, args = args } then join [if rec then "recursive " else "", "let ", name, " ", strJoin " " args]
    case ObjType { t = t } then join ["type ", name, match t with Some t then concat " : " t else ""]
    case ObjCon { t = t } then join ["con ", name, " : ", t]
    case ObjMexpr {} then "mexpr"
    case ObjProgram {} then ""
    case form then join [getFirstWord form, " ", name]
    end

-- Sets the (optional) type of a Let/Sem object, keeping other fields the same.
let objSetType = use ObjectForms in lam obj. lam ty.
    { obj with form = switch obj.form
    case ObjLet d then ObjLet { d with ty = ty }
    case ObjSem d then ObjSem { d with ty = ty }    
    case _ then obj.form end }

let objMerge : Object -> Object -> Object =
    use ObjectForms in
    lam obj1. lam obj2.
    let form = objFormMerge (objForm obj1) (objForm obj2) in
    objWithForm obj1 form
    
    
-- Object tree (hierarchy). Wraps Object to allow recursive nesting.
type ObjectTree
con ObjectNode : { obj: Object, children: [ObjectTree] } -> ObjectTree

-- Convenience helpers for ObjectTree.
let objTreeToString : ObjectTree -> String = lam tree. match tree with ObjectNode { obj = obj } in objToString obj.form obj.name

let objTreeObj : ObjectTree -> Object = lam tree. match tree with ObjectNode { obj = obj } in obj
let objTreeChildren : ObjectTree -> [ObjectTree] = lam tree. match tree with ObjectNode { children = children } in children

let objTreeWithObj : ObjectTree -> Object -> ObjectTree = lam tree. lam obj. match tree with ObjectNode d in ObjectNode { d with obj = obj }
let objTreeWithChildren : ObjectTree -> [ObjectTree] -> ObjectTree = lam tree. lam children. match tree with ObjectNode d in ObjectNode { d with children = children }

let objTreeDoc : ObjectTree -> String = lam tree. objDoc (objTreeObj tree)
let objTreeSourceCode : ObjectTree -> SourceCode = lam tree. objSourceCode (objTreeObj tree)
let objTreeName : ObjectTree -> String = lam tree. objName (objTreeObj tree)
let objTreeForm : ObjectTree -> use ObjectForms in ObjectForm = lam tree. objForm (objTreeObj tree)

let objTreeWithDoc : ObjectTree -> String -> ObjectTree = lam tree. lam doc.
    match tree with ObjectNode { obj = obj, children = children } in ObjectNode { obj = { obj with doc = doc}, children = children }
let objTreeWithSourceCode : ObjectTree -> SourceCode -> ObjectTree = lam tree. lam code.
    match tree with ObjectNode { obj = obj, children = children } in ObjectNode { obj = { obj with sourceCode = code}, children = children }
let objTreeRemoveChildren : ObjectTree -> ObjectTree = lam tree. ObjectNode { children = [], obj = objTreeObj tree }
