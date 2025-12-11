include "mexpr/ast.mc"
include "./syn-variant.mc"
include "../global/logger.mc"

-- Interface declaring all semantics for ObjectForms
lang ObjectFormInterface = MExprAst

    syn ObjectForm =

    sem objFormToString : ObjectForm -> String
    sem getFirstWord   : ObjectForm -> String
    sem objFormHasUrl  : ObjectForm -> Bool
    sem objFormHasLink : ObjectForm -> Bool
    sem objFormHasTests : ObjectForm -> Bool    
    sem objFormHasTests =
    | _ -> false

    sem objFormMergeFailed : ObjectForm -> ObjectForm -> ObjectForm
    sem objFormMergeFailed =
    | obj1 -> lam obj2.
            extractingWarn (join ["You cannot merge ", objFormToString obj1, " and ", objFormToString obj2, "."]);
            obj1

    sem objFormMerge : ObjectForm -> ObjectForm -> ObjectForm
    sem objFormMerge =
    | obj1 -> lam obj2. objFormMergeFailed obj1 obj2



end

----------------------------------------------------------------------
-- ObjProgram
----------------------------------------------------------------------
lang ObjProgramForm = ObjectFormInterface

    syn ObjectForm =
        | ObjProgram {}

    sem objFormToString =
        | ObjProgram {} -> "ObjProgram"

    sem getFirstWord =
        | ObjProgram {} -> ""

    sem objFormHasUrl =
        | ObjProgram {} -> true

    sem objFormHasLink =
        | ObjProgram {} -> true

end

----------------------------------------------------------------------
-- ObjInclude
----------------------------------------------------------------------
lang ObjIncludeForm = ObjectFormInterface

    syn ObjectForm =
        | ObjInclude { pathInFile: String }

    sem objFormToString =
        | ObjInclude { pathInFile = p } -> join ["ObjInclude, path = ", p]

    sem getFirstWord =
        | ObjInclude {} -> "include"

    sem objFormHasUrl =
        | ObjInclude {} -> false

    sem objFormHasLink =
        | ObjInclude {} -> true

end

----------------------------------------------------------------------
-- ObjLet
----------------------------------------------------------------------
lang ObjLetForm = ObjectFormInterface

    syn ObjectForm =
        | ObjLet { rec : Bool, args : [String], ty: Option Type }

    sem objFormToString =
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

    sem objFormHasUrl =
        | ObjLet {} -> true

    sem objFormHasLink =
        | ObjLet {} -> true

    sem objFormHasTests =
        | ObjLet {} -> true
end

----------------------------------------------------------------------
-- ObjLang
----------------------------------------------------------------------
lang ObjLangForm = ObjectFormInterface

    syn ObjectForm =
        | ObjLang { parents : [String] }

    sem objFormToString =
        | ObjLang { parents = parents } ->
            join ["ObjLang, parents: ", strJoin ", " parents]

    sem getFirstWord =
        | ObjLang {} -> "lang"

    sem objFormHasUrl =
        | ObjLang {} -> true

    sem objFormHasLink =
        | ObjLang {} -> true

end

----------------------------------------------------------------------
-- ObjType
----------------------------------------------------------------------
lang ObjTypeForm = ObjectFormInterface

    syn ObjectForm =
        | ObjType { t: Option String }

    sem objFormToString =
        | ObjType { t = t } ->
            join ["ObjType", match t with Some x then concat ", " x else ""]

    sem getFirstWord =
        | ObjType {} -> "type"

    sem objFormHasUrl =
        | ObjType {} -> true

    sem objFormHasLink =
        | ObjType {} -> true

    sem objFormMerge =
        | ObjType {} & obj1 -> lam obj2.
            match obj2 with ObjType {} then obj1
            else objFormMergeFailed obj1 obj2


end

----------------------------------------------------------------------
-- ObjUse
----------------------------------------------------------------------
lang ObjUseForm = ObjectFormInterface

    syn ObjectForm =
        | ObjUse {}

    sem objFormToString =
        | ObjUse {} -> "ObjUse"

    sem getFirstWord =
        | ObjUse {} -> "use"

    sem objFormHasUrl =
        | ObjUse {} -> false

    sem objFormHasLink =
        | ObjUse {} -> true

end

----------------------------------------------------------------------
-- ObjSem
----------------------------------------------------------------------
lang ObjSemForm = ObjectFormInterface

    syn ObjectForm =
        | ObjSem { langName: String, variants: [String], ty: Option Type }

    sem objFormToString =
        | ObjSem { langName = langName } ->
            join ["ObjSem, langName = ", langName]

    sem getFirstWord =
        | ObjSem {} -> "sem"

    sem objFormHasUrl =
        | ObjSem {} -> true

    sem objFormHasLink =
        | ObjSem {} -> true

    sem objFormMerge =
        | (ObjSem d1) & obj1 -> lam obj2.
            match obj2 with ObjSem d2 then
                ObjSem { d1 with variants = concat d1.variants d2.variants }
            else objFormMergeFailed obj1 obj2

end

----------------------------------------------------------------------
-- ObjSyn
----------------------------------------------------------------------
lang ObjSynForm = ObjectFormInterface

    syn ObjectForm =
        | ObjSyn { langName: String, variants: [SynVariant] }

    sem objFormToString =
        | ObjSyn { langName = langName } ->
            join ["ObjSyn, langName = ", langName]

    sem getFirstWord =
        | ObjSyn {} -> "syn"

    sem objFormHasUrl =
        | ObjSyn {} -> true

    sem objFormHasLink =
        | ObjSyn {} -> true

    sem objFormMerge =
        | (ObjSyn d1) & obj1 -> lam obj2.
            match obj2 with ObjSyn d2 then
                ObjSyn { d1 with variants = concat d1.variants d2.variants }
            else objFormMergeFailed obj1 obj2

end

----------------------------------------------------------------------
-- ObjCon
----------------------------------------------------------------------
lang ObjConForm = ObjectFormInterface

    syn ObjectForm =
        | ObjCon { t: String, parentType: String }

    sem objFormToString =
        | ObjCon { t = t, parentType = parentType } -> join ["ObjCon: ", t, " with parent: ", parentType]

    sem getFirstWord =
        | ObjCon {} -> "con"

    sem objFormHasUrl =
        | ObjCon {} -> true

    sem objFormHasLink =
        | ObjCon {} -> true

    sem objFormMerge =
        | ObjCon {} & obj1 -> lam obj2.
            match obj2 with ObjCon {} then obj1
            else objFormMergeFailed obj1 obj2

end

----------------------------------------------------------------------
-- ObjMexpr
----------------------------------------------------------------------
lang ObjMexprForm = ObjectFormInterface

    syn ObjectForm =
        | ObjMexpr {}

    sem objFormToString =
        | ObjMexpr {} -> "ObjMexpr"

    sem getFirstWord =
        | ObjMexpr {} -> "mexpr"

    sem objFormHasUrl =
        | ObjMexpr {} -> true

    sem objFormHasLink =
        | ObjMexpr {} -> true

end

----------------------------------------------------------------------
-- ObjUtest
----------------------------------------------------------------------
lang ObjUtestForm = ObjectFormInterface

    syn ObjectForm =
        | ObjUtest {}

    sem objFormToString =
        | ObjUtest {} -> "ObjUtest"

    sem getFirstWord =
        | ObjUtest {} -> "utest"

    sem objFormHasUrl =
        | ObjUtest {} -> true

    sem objFormHasLink =
        | ObjUtest {} -> true

end

----------------------------------------------------------------------
-- ObjRecursiveBloc
----------------------------------------------------------------------
lang ObjRecursiveBlocForm = ObjectFormInterface

    syn ObjectForm =
        | ObjRecursiveBloc {}

    sem objFormToString =
        | ObjRecursiveBloc {} -> "ObjRecursiveBloc"

    sem getFirstWord =
        | ObjRecursiveBloc {} -> "recursive"

    sem objFormHasUrl =
        | ObjRecursiveBloc {} -> false

    sem objFormHasLink =
        | ObjRecursiveBloc {} -> false

end

----------------------------------------------------------------------
-- Combine all object-kind languages
----------------------------------------------------------------------
lang ObjectForms =
    ObjProgramForm +
    ObjIncludeForm +
    ObjLetForm +
    ObjLangForm +
    ObjTypeForm +
    ObjUseForm +
    ObjSemForm +
    ObjSynForm +
    ObjConForm +
    ObjMexprForm +
    ObjUtestForm +
    ObjRecursiveBlocForm
end
