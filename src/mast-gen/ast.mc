include "mexpr/ast.mc"
include "./include-set.mc"

type Ast = use MExprAst in {
     expr: Expr,
     includeSet: IncludeSet String
}
