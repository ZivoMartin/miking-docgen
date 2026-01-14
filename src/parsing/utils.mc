include "mexpr/ast.mc"

recursive let getReturnType : use MExprAst in Type -> Type =
    lam t.
    use MExprAst in
    match t with TyArrow { to = to } then getReturnType to
    else t
end

-- Info(row1_1, col1_1), Info(row1_2, col1_2) -> Info(row1_1, col1_1, row1_2, col1_2)
let concatInfos : Info -> Info -> Info =
    lam i1. lam i2.
    match (i1, i2)
        with (Info { filename = filename, row1 = row1, col1 = col1 }, Info { row1 = row2, col1 = col2 })
        then Info { filename = filename, row1 = row1, col1 = col1, row2 = row2, col2 = col2 }
        else NoInfo ()
