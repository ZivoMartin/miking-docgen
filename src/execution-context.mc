include "./parsing/doc-tree.mc"
include "./parsing/ast.mc"
include "./extracting/objects.mc"

type ExecutionContext = { docTree : Option DocTree, ast: Option Ast, object: Option ObjectTree }

let execContextNew : () -> ExecutionContext = lam. { docTree = None {}, object = None {}, ast = None {} }

let execContextWithTree : ExecutionContext -> DocTree -> ExecutionContext = lam ctx. lam tree. { ctx with docTree = Some tree}
