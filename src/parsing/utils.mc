include "mexpr/ast.mc"

recursive let getReturnType : use MExprAst in Type -> Type =
    lam t.
    use MExprAst in
    match t with TyArrow { to = to } then getReturnType to
    else t
end

recursive let getLastNode : use MExprAst in Expr -> Expr =
    lam e.
    use MExprAst in
    match e with TmDecl { inexpr = inexpr } then getLastNode inexpr
    else e
end

let concatInfos : Info -> Info -> Info =
    lam i1. lam i2.
    match (i1, i2)
        with (Info { filename = filename, row1 = row1, col1 = col1 }, Info { row2 = row2, col2 = col2 })
        then Info { filename = filename, row1 = row1, col1 = col1, row2 = row2, col2 = col2 }
        else NoInfo ()
