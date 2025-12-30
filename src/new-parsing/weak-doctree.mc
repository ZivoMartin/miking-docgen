include "../parsing/doc-tree.mc"
include "../parsing/breaker-choosers.mc"
include "../parsing/lexing/token-readers.mc"

lang WeakDoctreeLang = BreakerChooser

     type NodeBloc = [Token]

     syn WeakDoctreeNode =
     | NodeProgram { nodes: [WeakDoctreeNode] }
     | NodeInclude { token : Token, tree : Option WeakDoctreeNode, path : String, isStdlib : Bool }
     | NodeToken Token
     | NodeLang [WeakDoctreeNode]

     | NodeLet { bloc: NodeBloc, rec: Bool }
     | NodeSyn NodeBloc
     | NodeSem NodeBloc
     | NodeUtest NodeBloc
     | NodeCon NodeBloc
     | NodeType NodeBloc
     | NodeMexpr NodeBloc

end

let doctree2weakDocTree : DocTree -> use WeakDoctreeLang in WeakDoctreeNode =
    use WeakDoctreeLang in
    lam doctree.

    type Res = { acc: [Token], nodes: [WeakDoctreeNode] } in

    recursive let work : DocTree -> Res =
        lam doctree.
        switch doctree
        case DocTreeLeaf { token = token } then
             { acc = [token], nodes = [NodeToken token] }

        case DocTreeNode { children = children, state = state, token = token } then
        
            let sub = foldl (
                lam acc. lam child.
                match work child with { acc = subacc, nodes = nodes } in
                { tokens = concat (reverse subacc) acc.tokens, nodes = concat (reverse nodes) acc.nodes }
            ) { nodes = [], tokens = [token] } children in

            match sub with { nodes = nodes, tokens = bloc } in

            let nodes = switch state
            case StateProgram {} then [NodeProgram { nodes = nodes }]
            case StateLang {} then [NodeLang nodes]
            case StateRec {} then nodes

            case StateTopLet {} | StateLet {} then [NodeLet { rec = false, bloc = bloc }]
            case StateRecLet {} | StateTopRec {} then [NodeLet { rec = true, bloc = bloc }]
            case StateTopUse {} | StateUse {} then []
            case StateTopType {} | StateType {} then [NodeType bloc]
            case StateSem {} then [NodeSem bloc]
            case StateSyn {} then [NodeSyn bloc]
            case StateCon {} | StateTopCon {} then [NodeCon bloc]
            case StateMexpr {} then [NodeMexpr bloc]
            case StateUtest {} | StateTopUtest {} then [NodeUtest bloc]
            end in

            { acc = [], nodes = [] }

        case DocTreeIncludeNode {
             token = token,
             tree = tree,
             path = path,
             isStdlib = isStdlib
             } then
               let tree = optionMap (lam tree. head (work tree).nodes)  tree in
               { acc = [token], nodes = [NodeInclude { token = token, tree = tree, path = path, isStdlib = isStdlib }]}
        end
    in

    head (work doctree).nodes

