include "mexpr/ast.mc"
include "./syn-variant.mc"
include "../global/logger.mc"
include "./source-code-builder.mc"

-- Interface declaring all semantics for Objects
lang ObjectInterface = MExprAst

    type ObjectDatas = {
        name: String,
        doc : String,
        namespace: String,
        sourceCode: SourceCode,
        isStdlib: Bool,
        renderIt: Bool,
        id: Int
    }

    type ObjectChildren = [Object]

    syn Object =

    sem objGetDatas : Object -> ObjectDatas
    sem objGetChildren : Object -> ObjectChildren
    sem objGetChildren =
    | obj -> []

    sem objSetDatas : Object -> ObjectDatas -> Object
    sem objSetChildren : Object -> ObjectDatas -> Object
    sem objSetChildren =
    | obj -> lam. obj

    sem objToString : Object -> String
    sem getFirstWord : Object -> String
    sem objHasUrl  : Object -> Bool
    sem objHasLink : Object -> Bool
    sem objHasTests : Object -> Bool    
    sem objHasTests =
    | _ -> false

    sem objGetLangName : Object -> String
    sem objGetLangName =
    | _ -> ""

    sem objSetType : Object -> Option Type -> Object
    sem objSetType =
    | obj -> lam. obj

    sem objMergeFailed : Object -> Object -> Object
    sem objMergeFailed =
    | obj1 -> lam obj2.
            extractingWarn (join ["You cannot merge ", objToString obj1, " and ", objToString obj2, "."]);
            obj1

    sem objMerge : Object -> Object -> Object
    sem objMerge =
    | obj1 -> lam obj2. objMergeFailed obj1 obj2

    sem objPrettyPrint : Object -> String
    sem objPrettyPrint =
    | obj -> join [getFirstWord obj, " ", objName obj]

    sem objSetField : Object -> (ObjectDatas -> ObjectDatas) -> Object
    sem objSetField =
    | obj -> lam setter. objSetDatas obj (setter (objGetDatas obj))

    -- Simple field accessors.
    sem objName = | obj -> (objGetDatas obj).name
    sem objDoc = | obj -> (objGetDatas obj).doc
    sem objSourceCode = | obj -> (objGetDatas obj).sourceCode
    sem objNamespace = | obj -> (objGetDatas obj).namespace
    sem objIsStdlib = | obj -> (objGetDatas obj).isStdlib
    sem objRenderIt = | obj -> (objGetDatas obj).renderIt
    sem objId = | obj -> (objGetDatas obj).id

    -- Object updaters (immutable setters).
    sem objWithName =
    | obj -> lam name. objSetField obj (lam d. { d with name = name })

    sem objWithDoc =
    | obj -> lam doc. objSetField obj (lam d. { d with doc = doc })

    sem objWithIsStdlib =
    | obj -> lam isStdlib. objSetField obj (lam d. { d with isStdlib = isStdlib })

    sem objWithSourceCode =
    | obj -> lam sourceCode. objSetField obj (lam d. { d with sourceCode = sourceCode })

    sem objWithRenderIt =
    | obj -> lam renderIt. objSetField obj (lam d. { d with renderIt = renderIt })

    sem objWithId =
    | obj -> lam id. objSetField obj (lam d. { d with id = id })

    -- Sets a shorter namespace by removing `prefix`; stores the prefix for recovery.
    -- Warns if the namespace does not start with the given prefix.
    sem objWithPrefix =
    | obj -> lam prefix.
        let process = lam.
            let basePrefix = objNamespace obj in
            let lengthBasePrefix = length basePrefix in
            let lengthPrefix = length prefix in

            if objIsStdlib obj then basePrefix
            else if strStartsWith prefix basePrefix then
                subsequence basePrefix lengthPrefix lengthBasePrefix
            else
                error (join ["The namespace ", basePrefix, " does not start with the prefix ", prefix, "."])
        in
        let namespace = match prefix with "" then objNamespace obj else process () in
        let namespace =
            if strStartsWith "/" namespace then namespace
            else cons '/' namespace
        in

        objSetField obj (lam d. { d with namespace = namespace })

    -- Replaces namespace; strips stdlib prefix if present; re-applies stored `prefix`.
    sem objWithNamespace =
    | obj -> lam namespace.
        let namespace =
            if strStartsWith stdlibLoc namespace then
                subsequence namespace (length stdlibLoc) (length namespace)
            else
                namespace
        in
        
        objSetField obj (lam d. { d with namespace = namespace })

    -- Returns true if the object has a meaningful id.
    sem objHasId =
    | obj -> neqi (objId obj) 0

    -- Returns true if the object has a code source (otherwise it has probably been added during naming)
    sem objHasSourceCode =
    | obj -> not (sourceCodeIsEmpty (objSourceCode obj))

    -- Returns absolute path = prefix + namespace.
    sem objAbsolutePath =
    | obj -> lam prefix.
        concat prefix (objNamespace obj)

    sem objDefaultDoc : () -> String
    sem objDefaultDoc =
    | _ -> "No documentation available here."

    -- Empty default object (neutral values).
    sem defaultDatas : () -> ObjectDatas
    sem defaultDatas =
    | () -> {
        name = "",
        doc = "",
        namespace = "",
        renderIt = false,
        isStdlib = false,
        sourceCode = sourceCodeEmpty (),
        id = 0
    }

    sem objTryGetDoc : Object -> String
    sem objTryGetDoc =
    | obj ->
        let doc = objDoc obj in
        if eqString doc (objDefaultDoc ()) then "" else doc

    
end

----------------------------------------------------------------------
-- ObjProgram
----------------------------------------------------------------------
lang ObjProgram = ObjectInterface

    syn Object =
    | ObjProgram { children: ObjectChildren, datas: ObjectDatas}

    sem objGetDatas =
    | ObjProgram { datas = datas } -> datas

    sem objGetChildren =
    | ObjProgram { children = children } -> children

    sem objSetDatas =
    | ObjProgram f -> lam datas. { f with datas = datas }

    sem objSetChildren =
    | ObjProgram f -> lam children. { f with children = children }

    sem objToString =
    | ObjProgram {} -> "ObjProgram"

    sem getFirstWord =
    | ObjProgram {} -> ""

    sem objHasUrl =
    | ObjProgram {} -> true

    sem objPrettyPrint =
    | ObjProgram {} -> ""

    sem objHasLink =
    | ObjProgram {} -> true

end

----------------------------------------------------------------------
-- ObjInclude
----------------------------------------------------------------------
lang ObjInclude = ObjectInterface

    syn Object =
    | ObjInclude { pathInFile: String, datas: ObjectDatas, child: Object }

    sem objSetDatas =
    | ObjInclude f -> lam datas. ObjInclude { f with datas = datas}

    sem objSetChildren =
    | ObjInclude f & obj -> lam children.
      if neqi 1 (length children) then extractingWarn "Inlude nodes should only have one children"; obj
      else ObjInclude { f with child = head children}

    sem objGetChildren =
    | ObjInclude { child = child } -> [child]

    sem objGetDatas =
    | ObjInclude { datas = datas } -> datas

    sem objToString =
    | ObjInclude { pathInFile = p } -> join ["ObjInclude, path = ", p]

    sem getFirstWord =
    | ObjInclude {} -> "include"

    sem objHasUrl =
    | ObjInclude {} -> false

    sem objHasLink =
    | ObjInclude {} -> true

end

----------------------------------------------------------------------
-- ObjLet
----------------------------------------------------------------------
lang ObjLet = ObjectInterface

    syn Object =
    | ObjLet { rec : Bool, args : [String], ty: Option Type, datas: ObjectDatas }

    sem objGetDatas =
    | ObjLet { datas = datas } -> datas

    sem objSetDatas =
    | ObjLet f -> lam datas. ObjLet { f with datas = datas}

    sem objToString =
    | ObjLet { rec = rec, args = args, ty = ty } ->
            join [
                "ObjLet, recursive: ",
                bool2string rec,
                ", args: [",
                strJoin ", " args,
                "]"
            ]

    sem getFirstWord =
    | ObjLet {} -> "let"

    sem objHasUrl =
    | ObjLet {} -> true

    sem objHasLink =
    | ObjLet {} -> true

    sem objSetType =
    | ObjLet d -> lam ty. ObjLet { d with ty = ty }

    sem objPrettyPrint =
    | ObjLet { rec = rec, args = args } & obj ->
      join [if rec then "recursive " else "", "let ", objName obj, " ", strJoin " " args]

    sem objHasTests =
    | ObjLet {} -> true
end

----------------------------------------------------------------------
-- ObjLang
----------------------------------------------------------------------
lang ObjLang = ObjectInterface

    syn Object =
    | ObjLang { parents : [String], datas : ObjectDatas, children: ObjectChildren }

    sem objSetDatas =
    | ObjLang f -> lam datas. ObjLang { f with datas = datas}

    sem objSetChildren =
    | ObjLang f -> lam children. { f with children = children }

    sem objGetDatas =
    | ObjLang { datas = datas } -> datas

    sem objGetChildren =
    | ObjLang { children = children } -> children

    sem objToString =
    | ObjLang { parents = parents } ->
            join ["ObjLang, parents: ", strJoin ", " parents]

    sem getFirstWord =
    | ObjLang {} -> "lang"

    sem objHasUrl =
    | ObjLang {} -> true

    sem objHasLink =
    | ObjLang {} -> true

end

----------------------------------------------------------------------
-- ObjType
----------------------------------------------------------------------
lang ObjType = ObjectInterface

    syn Object =
    | ObjType { t: Option String, datas: ObjectDatas }

    sem objGetDatas =
    | ObjType { datas = datas } -> datas

    sem objSetDatas =
    | ObjType f -> lam datas. ObjType { f with datas = datas }

    sem objToString =
    | ObjType { t = t } ->
        join ["ObjType", match t with Some x then concat ", " x else ""]

    sem getFirstWord =
    | ObjType {} -> "type"

    sem objHasUrl =
    | ObjType {} -> true

    sem objHasLink =
    | ObjType {} -> true


    sem objPrettyPrint =
    | ObjType { t = t } & obj ->
      join ["type ", objName obj, match t with Some t then concat " : " t else ""]

    sem objMerge =
    | ObjType {} & obj1 -> lam obj2.
            match obj2 with ObjType {} then obj1
            else objMergeFailed obj1 obj2


end

----------------------------------------------------------------------
-- ObjSem
----------------------------------------------------------------------
lang ObjSem = ObjectInterface

    syn Object =
    | ObjSem { langName: String, variants: [String], ty: Option Type, datas: ObjectDatas }

    sem objToString =
    | ObjSem { langName = langName } ->
            join ["ObjSem, langName = ", langName]

    sem objGetDatas =
    | ObjSem { datas = datas } -> datas

    sem objSetDatas =
    | ObjSem f -> lam datas. ObjSem { f with datas = datas }

    sem getFirstWord =
    | ObjSem {} -> "sem"

    sem objHasUrl =
    | ObjSem {} -> true

    sem objHasLink =
    | ObjSem {} -> true

    sem objGetLangName =
    | ObjSem { langName = langName } -> langName

    sem objSetType =
    | ObjSem d -> lam ty. ObjSem { d with ty = ty }    

    sem objMerge =
    | (ObjSem d1) & obj1 -> lam obj2.
            match obj2 with ObjSem d2 then
                ObjSem { d1 with variants = concat d1.variants d2.variants }
            else objMergeFailed obj1 obj2

end

----------------------------------------------------------------------
-- ObjSyn
----------------------------------------------------------------------
lang ObjSyn = ObjectInterface

    syn Object =
    | ObjSyn { langName: String, variants: [SynVariant], datas: ObjectDatas }

    sem objGetDatas =
    | ObjSyn { datas = datas } -> datas

    sem objSetDatas =
    | ObjSyn f -> lam datas. ObjSyn { f with datas = datas }

    sem objToString =
    | ObjSyn { langName = langName } ->
            join ["ObjSyn, langName = ", langName]

    sem getFirstWord =
    | ObjSyn {} -> "syn"

    sem objHasUrl =
    | ObjSyn {} -> true

    sem objGetLangName =
    | ObjSyn { langName = langName } -> langName

    sem objHasLink =
    | ObjSyn {} -> true

    sem objMerge =
    | (ObjSyn d1) & obj1 -> lam obj2.
            match obj2 with ObjSyn d2 then
                ObjSyn { d1 with variants = concat d1.variants d2.variants }
            else objMergeFailed obj1 obj2

end

----------------------------------------------------------------------
-- ObjCon
----------------------------------------------------------------------
lang ObjCon = ObjectInterface

    syn Object =
    | ObjCon { t: String, parentType: String, datas: ObjectDatas }

    sem objGetDatas =
    | ObjCon { datas = datas } -> datas

    sem objSetDatas =
    | ObjCon f -> lam datas. ObjCon { f with datas = datas }

    sem objToString =
    | ObjCon { t = t, parentType = parentType } -> join ["ObjCon: ", t, " with parent: ", parentType]

    sem getFirstWord =
    | ObjCon {} -> "con"

    sem objHasUrl =
    | ObjCon {} -> true

    sem objHasLink =
    | ObjCon {} -> true


    sem objPrettyPrint =
    | ObjCon { t = t } & obj -> join ["con ", objName obj, " : ", t]

    sem objMerge =
    | ObjCon {} & obj1 -> lam obj2.
            match obj2 with ObjCon {} then obj1
            else objMergeFailed obj1 obj2

end

----------------------------------------------------------------------
-- ObjMexpr
----------------------------------------------------------------------
lang ObjMexpr = ObjectInterface

    syn Object =
    | ObjMexpr ObjectDatas

    sem objGetDatas =
    | ObjMexpr datas -> datas

    sem objSetDatas =
    | ObjMexpr _ -> lam datas. ObjMexpr datas

    sem objToString =
    | ObjMexpr {} -> "ObjMexpr"

    sem getFirstWord =
    | ObjMexpr {} -> "mexpr"

    sem objHasUrl =
    | ObjMexpr {} -> true

    sem objPrettyPrint =
    | ObjMexpr {} -> "mexpr"

    sem objHasLink =
    | ObjMexpr {} -> true

end

----------------------------------------------------------------------
-- ObjUtest
----------------------------------------------------------------------
lang ObjUtest = ObjectInterface

    syn Object =
    | ObjUtest ObjectDatas

    sem objGetDatas =
    | ObjUtest datas -> datas

    sem objSetDatas =
    | ObjUtest _ -> lam datas. ObjUtest datas

    sem objToString =
    | ObjUtest {} -> "ObjUtest"

    sem getFirstWord =
    | ObjUtest {} -> "utest"

    sem objHasUrl =
    | ObjUtest {} -> true

    sem objHasLink =
    | ObjUtest {} -> true

end

----------------------------------------------------------------------
-- Combine all object-kind languages
----------------------------------------------------------------------
lang Objects =
    ObjProgram +
    ObjInclude +
    ObjLet +
    ObjLang +
    ObjType +
    ObjSem +
    ObjSyn +
    ObjCon +
    ObjMexpr +
    ObjUtest
end
