include "mexpr/ast.mc"
include "./syn-variant.mc"
include "../global/logger.mc"

-- Interface declaring all semantics for ObjectKinds
lang ObjectKindInterface = MExprAst

    syn ObjectKind =

    sem objKindToString : ObjectKind -> String
    sem getFirstWord   : ObjectKind -> String
    sem objKindHasUrl  : ObjectKind -> Bool
    sem objKindHasLink : ObjectKind -> Bool

    sem objKindMergeFailed : ObjectKind -> ObjectKind -> ObjectKind
    sem objKindMergeFailed =
    | obj1 -> lam obj2.
            extractingWarn (join ["You cannot merge ", objKindToString obj1, " and ", objKindToString obj2, "."]);
            obj1

    sem objKindMerge : ObjectKind -> ObjectKind -> ObjectKind
    sem objKindMerge =
    | obj1 -> lam obj2. objKindMergeFailed obj1 obj2



end

----------------------------------------------------------------------
-- ObjProgram
----------------------------------------------------------------------
lang ObjProgramKind = ObjectKindInterface

    syn ObjectKind =
        | ObjProgram {}

    sem objKindToString =
        | ObjProgram {} -> "ObjProgram"

    sem getFirstWord =
        | ObjProgram {} -> ""

    sem objKindHasUrl =
        | ObjProgram {} -> true

    sem objKindHasLink =
        | ObjProgram {} -> true

end

----------------------------------------------------------------------
-- ObjInclude
----------------------------------------------------------------------
lang ObjIncludeKind = ObjectKindInterface

    syn ObjectKind =
        | ObjInclude { pathInFile: String }

    sem objKindToString =
        | ObjInclude { pathInFile = p } -> join ["ObjInclude, path = ", p]

    sem getFirstWord =
        | ObjInclude {} -> "include"

    sem objKindHasUrl =
        | ObjInclude {} -> false

    sem objKindHasLink =
        | ObjInclude {} -> true

end

----------------------------------------------------------------------
-- ObjLet
----------------------------------------------------------------------
lang ObjLetKind = ObjectKindInterface

    syn ObjectKind =
        | ObjLet { rec : Bool, args : [String], ty: Option Type }

    sem objKindToString =
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

    sem objKindHasUrl =
        | ObjLet {} -> true

    sem objKindHasLink =
        | ObjLet {} -> true

end

----------------------------------------------------------------------
-- ObjLang
----------------------------------------------------------------------
lang ObjLangKind = ObjectKindInterface

    syn ObjectKind =
        | ObjLang { parents : [String] }

    sem objKindToString =
        | ObjLang { parents = parents } ->
            join ["ObjLang, parents: ", strJoin ", " parents]

    sem getFirstWord =
        | ObjLang {} -> "lang"

    sem objKindHasUrl =
        | ObjLang {} -> true

    sem objKindHasLink =
        | ObjLang {} -> true

end

----------------------------------------------------------------------
-- ObjType
----------------------------------------------------------------------
lang ObjTypeKind = ObjectKindInterface

    syn ObjectKind =
        | ObjType { t: Option String }

    sem objKindToString =
        | ObjType { t = t } ->
            join ["ObjType", match t with Some x then concat ", " x else ""]

    sem getFirstWord =
        | ObjType {} -> "type"

    sem objKindHasUrl =
        | ObjType {} -> true

    sem objKindHasLink =
        | ObjType {} -> true

    sem objKindMerge =
        | ObjType {} & obj1 -> lam obj2.
            match obj2 with ObjType {} then obj1
            else objKindMergeFailed obj1 obj2


end

----------------------------------------------------------------------
-- ObjUse
----------------------------------------------------------------------
lang ObjUseKind = ObjectKindInterface

    syn ObjectKind =
        | ObjUse {}

    sem objKindToString =
        | ObjUse {} -> "ObjUse"

    sem getFirstWord =
        | ObjUse {} -> "use"

    sem objKindHasUrl =
        | ObjUse {} -> false

    sem objKindHasLink =
        | ObjUse {} -> true

end

----------------------------------------------------------------------
-- ObjSem
----------------------------------------------------------------------
lang ObjSemKind = ObjectKindInterface

    syn ObjectKind =
        | ObjSem { langName: String, variants: [String], ty: Option Type }

    sem objKindToString =
        | ObjSem { langName = langName } ->
            join ["ObjSem, langName = ", langName]

    sem getFirstWord =
        | ObjSem {} -> "sem"

    sem objKindHasUrl =
        | ObjSem {} -> true

    sem objKindHasLink =
        | ObjSem {} -> true

    sem objKindMerge =
        | (ObjSem d1) & obj1 -> lam obj2.
            match obj2 with ObjSem d2 then
                ObjSem { d1 with variants = concat d1.variants d2.variants }
            else objKindMergeFailed obj1 obj2

end

----------------------------------------------------------------------
-- ObjSyn
----------------------------------------------------------------------
lang ObjSynKind = ObjectKindInterface

    syn ObjectKind =
        | ObjSyn { langName: String, variants: [SynVariant] }

    sem objKindToString =
        | ObjSyn { langName = langName } ->
            join ["ObjSyn, langName = ", langName]

    sem getFirstWord =
        | ObjSyn {} -> "syn"

    sem objKindHasUrl =
        | ObjSyn {} -> true

    sem objKindHasLink =
        | ObjSyn {} -> true

    sem objKindMerge =
        | (ObjSyn d1) & obj1 -> lam obj2.
            match obj2 with ObjSyn d2 then
                ObjSyn { d1 with variants = concat d1.variants d2.variants }
            else objKindMergeFailed obj1 obj2

end

----------------------------------------------------------------------
-- ObjCon
----------------------------------------------------------------------
lang ObjConKind = ObjectKindInterface

    syn ObjectKind =
        | ObjCon { t: String }

    sem objKindToString =
        | ObjCon { t = t } -> join ["ObjCon: ", t]

    sem getFirstWord =
        | ObjCon {} -> "con"

    sem objKindHasUrl =
        | ObjCon {} -> true

    sem objKindHasLink =
        | ObjCon {} -> true

    sem objKindMerge =
        | ObjCon {} & obj1 -> lam obj2.
            match obj2 with ObjCon {} then obj1
            else objKindMergeFailed obj1 obj2

end

----------------------------------------------------------------------
-- ObjMexpr
----------------------------------------------------------------------
lang ObjMexprKind = ObjectKindInterface

    syn ObjectKind =
        | ObjMexpr {}

    sem objKindToString =
        | ObjMexpr {} -> "ObjMexpr"

    sem getFirstWord =
        | ObjMexpr {} -> "mexpr"

    sem objKindHasUrl =
        | ObjMexpr {} -> true

    sem objKindHasLink =
        | ObjMexpr {} -> true

end

----------------------------------------------------------------------
-- ObjUtest
----------------------------------------------------------------------
lang ObjUtestKind = ObjectKindInterface

    syn ObjectKind =
        | ObjUtest {}

    sem objKindToString =
        | ObjUtest {} -> "ObjUtest"

    sem getFirstWord =
        | ObjUtest {} -> "utest"

    sem objKindHasUrl =
        | ObjUtest {} -> true

    sem objKindHasLink =
        | ObjUtest {} -> true

end

----------------------------------------------------------------------
-- ObjRecursiveBloc
----------------------------------------------------------------------
lang ObjRecursiveBlocKind = ObjectKindInterface

    syn ObjectKind =
        | ObjRecursiveBloc {}

    sem objKindToString =
        | ObjRecursiveBloc {} -> "ObjRecursiveBloc"

    sem getFirstWord =
        | ObjRecursiveBloc {} -> "recursive"

    sem objKindHasUrl =
        | ObjRecursiveBloc {} -> false

    sem objKindHasLink =
        | ObjRecursiveBloc {} -> false

end

----------------------------------------------------------------------
-- Combine all object-kind languages
----------------------------------------------------------------------
lang ObjectKinds =
    ObjProgramKind +
    ObjIncludeKind +
    ObjLetKind +
    ObjLangKind +
    ObjTypeKind +
    ObjUseKind +
    ObjSemKind +
    ObjSynKind +
    ObjConKind +
    ObjMexprKind +
    ObjUtestKind +
    ObjRecursiveBlocKind
end
