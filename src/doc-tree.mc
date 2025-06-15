include "breaker-choosers.mc"

type DocTree
con Node : use TokenReader in use BreakerChooser in { sons: [DocTree], token: Token, state: State} -> DocTree
con Leaf : use TokenReader in use BreakerChooser in (Token, State) -> DocTree

-- Takes a tree and returns the md file documenting the tree
-- Avoir un buffer qui preserve le long du parcours les commentaires sur des lignes consecutives reperés. Le vider si l'on croise un retour à la ligne dans un sep ou un word quelconque, le mettre dans la doc si l'on croise un noeud, et le vider juste après
let compileToMd : (DocTree -> String -> ()) = lam tree. lam repoPath.
    use TokenReader in use BreakerChooser in
    recursive
    let compileToMd : (DocTree -> String -> [String] -> [String]) =
    lam tree. lam currentPath. lam currentComment.
    
        match tree with Node { sons = sons, token = token, state = state } then
            match state with Program {} then
                error "TODO: Extraire les premiers fils tant qu'ils s'agit de commentaires, c'est la doc du programme"
            else match state with (Use {} | TopUse {}) then
                error "TODO"
            else match state with (Let {} | TopLet {}) then
                error "TODO"
            else match state with (Type {} | TopType {}) then
                error "TODO"
            else match state with Sem {} then
                error "TODO"
            else match state with Syn {} then
                error "TODO"
            else match state with (Con {} | TopCon {}) then
                error "TODO"
            else match state with Mexpr {} then
                error "TODO"
            else never
    
        else match tree with Leaf (token, state) then
            match token with Comment { content = content } then
                cons currentComment content
            else match token with Include { content = content } then
                error "TODO: Parse a new file"
            else
                [] -- Emptying the buffer
        else never
    in let res = compileToMd tree "" in ()
    
let displayTree : (DocTree -> ()) = use TokenReader in use BreakerChooser in lam tree.
  recursive let replicate = lam n. lam str.
        if eqi n 0 then [] else cons str (replicate (subi n 1) str) in
    
  let indentString = lam n.
    if eqi n 0 then "" else
      concatAll (replicate n "  ")
  in
  recursive let displayTreeIndented = lam tree. lam depth.
    match tree with Node { sons = sons, token = token, state = state } then
         print (concatAll [indentString depth, "Node (", toString state, "): ", lit token, "\n"]);
        iter (lam child. displayTreeIndented child (addi depth 1)) sons
    else match tree with Leaf (token, state) then
        match token with Separator {} | Eof {} then () else 
            print (concatAll [indentString depth, "Leaf: ", lit token, "\n"])
    else never
  in
  displayTreeIndented tree 0
