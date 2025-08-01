include "./parsing/doc-tree.mc"
include "./extracting/objects.mc"
    
type ExecutionContext = { docTree : Option DocTree, object: Option ObjectTree }

let execContextNew : () -> ExecutionContext = lam. { docTree = None {}, object = None {} }

let execContextWithTree : ExecutionContext -> DocTree -> ExecutionContext = lam ctx. lam tree. { ctx with docTree = Some tree}
