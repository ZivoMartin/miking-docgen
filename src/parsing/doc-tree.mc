-- # DocTree Utilities
--
-- This module defines:
-- - a `DocTree` structure to represent a document as a tree of tokens and formatting state.
-- - a `displayTree` function to print this tree with indentation.
--
-- A `Leaf` holds a single token and its state.
-- A `Node` holds children (sons), a token, and a state.
-- The display function prints the hierarchy clearly for debugging or visualization.

include "./breaker-choosers.mc"
include "../logger.mc"

-- Main DocTree types, will contains Leafs and Nodes
type DocTree
-- A single token with formatting state
con Leaf : use TokenReader in use BreakerChooser in { token: Token, state: State, pos: Pos } -> DocTree
-- A node with children, token and state
con Node : use TokenReader in use BreakerChooser in { sons: [DocTree], token: Token, state: State, pos: Pos } -> DocTree
-- A node representing an include, contains the DocTree of the file.
con IncludeNode : use TokenReader in use BreakerChooser in { token: Token, tree: Option DocTree, state: State, path : String, isStdlib : Bool, pos: Pos } -> DocTree

-- Print the document tree with indentation
let displayTree : (DocTree -> ()) = use TokenReader in use BreakerChooser in lam tree.
    -- Helper to repeat a string n times
    recursive let replicate = lam n. lam str.
        if eqi n 0 then [] else cons str (replicate (subi n 1) str) in
    
    -- Build indentation string
    let indentString = lam n.
        if eqi n 0 then "" else
            join (replicate n "  ")
    in
    
    -- Recursive print of tree with current indentation depth
    recursive let displayTreeIndented = lam tree. lam depth.
            
        switch tree
        case Node { sons = sons, token = token, state = state } then
            printLn (join [indentString depth, "Node (", toString state, ")"]);
            iter (lam child. displayTreeIndented child (addi depth 1)) sons
        case Leaf { token = token, state = state } then
            -- Skip separators and EOF for cleaner output
            match token with Separator {} | Eof {} then () else 
                printLn (join [indentString depth, "Leaf (", tokenToString token, "):", lit token])
        case IncludeNode { tree = tree, token = token, state = state, path = path } then
            printLn (join [indentString depth, "Include \"", path, "\" (", toString state, ")"]);
            match tree with Some tree then displayTreeIndented tree (addi depth 1) else ()
        case _ then parsingWarn "No-covered variant in DocTree reached during displayTree execution."
        end
    in
    
    displayTreeIndented tree 0
