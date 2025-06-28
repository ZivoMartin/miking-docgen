include "source-code-builder.mc"
include "objects.mc"
include "extracter.mc"
include "../parsing/parser.mc"

type TreeSourceCode
con TreeSourceCodeNode : [TreeSourceCode] -> TreeSourceCode
con TreeSourceCodeSnippet : String -> TreeSourceCode

let getTreeSourceCode : ObjectTree -> TreeSourceCode = lam tree. TreeSourceCodeSnippet ""
    
let getRawSourceCode : ObjectTree -> String = lam tree.
    recursive let work : ObjectTree -> String = lam tree.
        type Arg = { sons: [ObjectTree], s: String  } in
        switch tree
        case ObjectNode { sons = sons, obj = { sourceCode = sourceCode } } then
            let sons = objectSonsFilterNodes sons in
            (foldl
                (lam a: Arg. lam word: SourceCodeWord.
                    match word with Some w then
                        { a with s = concat w a.s }
                    else
                        match sons with [son] ++ sons then
                            let sonCode = work son in
                            { s = concat sonCode a.s, sons = sons }
                        else
                            warn "Sons should never be empty at this point.";
                            a
                ) { sons = sons, s = "" } sourceCode).s
        case ObjectLeaf s then ""
        end in
    work tree


