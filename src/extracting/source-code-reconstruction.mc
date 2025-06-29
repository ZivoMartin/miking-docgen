include "source-code-builder.mc"
include "objects.mc"
include "extracter.mc"
include "../parsing/parser.mc"

type TreeSourceCode
con TreeSourceCodeNode : [TreeSourceCode] -> TreeSourceCode
con TreeSourceCodeSnippet : [SourceCodeWord] -> TreeSourceCode

let getTreeSourceCode : ObjectTree -> TreeSourceCode = lam tree.
    recursive let work : ObjectTree -> TreeSourceCode = lam tree.
        type Arg = { sons: [ObjectTree], tree: [TreeSourceCode], buffer: [SourceCodeWord] } in
        switch tree
        case ObjectNode { sons = sons, obj = { sourceCode = sourceCode } } then
            let sons = objectSonsFilterNodes sons in
            let foldResult = foldl
                (lam a: Arg. lam word: Option SourceCodeWord.
                    match word with Some w then
                        { a with buffer = cons w a.buffer }
                    else
                        match a.sons with [son] ++ sons then
                            let sonCode = work son in 
                            let tree =
                                match s with "" then
                                    cons sonCode a.tree
                                else
                                    concat [sonCode, TreeSourceCodeSnippet a.buffer] a.tree in
                            { buffer = "", tree = tree, sons = sons }
                        else
                            warn "Sons should never be empty at this point.";
                            a
                ) { sons = sons, tree = [], buffer = "" } sourceCode in
            switch (foldResult.tree, foldResult.buffer)
            case ([], buffer) then TreeSourceCodeNode [TreeSourceCodeSnippet buffer]
            case (tree, "") then TreeSourceCodeNode tree
            case (tree, buffer) then TreeSourceCodeNode (cons (TreeSourceCodeSnippet buffer) tree)
            end
        case ObjectLeaf s then
            warn "getTreeSourceCode: We are not supposed to reach this point.";
            TreeSourceCodeSnippet ""
        end in
    work tree

    
let getRawSourceCode : ObjectTree -> SourceCodeWord = lam tree.
    recursive let work : ObjectTree -> SourceCodeWord = lam tree.
        type Arg = { sons: [ObjectTree], s: String  } in
        switch tree
        case ObjectNode { sons = sons, obj = { sourceCode = sourceCode } } then
            let sons = objectSonsFilterNodes sons in
            (foldl
                (lam a: Arg. lam word: SourceCodeWord.
                    match word with Some w then
                        { a with s = concat w a.s }
                    else
                        match a.sons with [son] ++ sons then
                            let sonCode = work son in
                            { s = concat sonCode a.s, sons = sons }
                        else
                            warn "Sons should never be empty at this point.";
                            a
                ) { sons = sons, s = "" } sourceCode).s
        case ObjectLeaf s then
            warn "getRawSourceCode: We are not supposed to reach this point.";
            ""
        end in
    work tree


