include "breaker-choosers.mc"
    
type DocTree
con Leaf : use TokenReader in use BreakerChooser in { token: Token, state: State } -> DocTree
con Node : use TokenReader in use BreakerChooser in { sons: [DocTree], token: Token, state: State} -> DocTree


let displayTree : (DocTree -> ()) = use TokenReader in use BreakerChooser in lam tree.
  recursive let replicate = lam n. lam str.
        if eqi n 0 then [] else cons str (replicate (subi n 1) str) in
    
  let indentString = lam n.
    if eqi n 0 then "" else
      concatAll (replicate n "  ")
  in
  recursive let displayTreeIndented = lam tree. lam depth.
    match tree with Node { sons = sons, token = token, state = state } then
         printLn (concatAll [indentString depth, "Node (", toString state, ")"]);
        iter (lam child. displayTreeIndented child (addi depth 1)) sons
    else match tree with Leaf { token = token, state = state } then
        match token with Separator {} | Eof {} then () else 
            printLn (concatAll [indentString depth, "Leaf (", tokenToString token, "):", lit token])
    else never
  in
  displayTreeIndented tree 0
