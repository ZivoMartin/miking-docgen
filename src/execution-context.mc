include "./parsing/doc-tree.mc"
include "./mast-gen/ast.mc"
include "./extracting/objects.mc"
include "./options/options.mc"

type ExecutionContext = use TokenReader in {    
     opt: Options,
     mainFile: String,
     tokens: [Token],
     docTree : Option DocTree,
     ast: Option Ast,
     object: Option ObjectTree
}

let execContextNew : Options -> ExecutionContext = lam. {
    opt = opt,
    mainFile = opt.file,
    tokens = [],
    docTree = None {},
    object = None {},
    ast = None {}
}

let execContextWithTree : ExecutionContext -> DocTree -> ExecutionContext = lam ctx. lam tree. { ctx with docTree = Some tree}
