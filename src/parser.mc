include "token-readers.mc"
include "string.mc"

type DocTree
con Node : { sons: [DocTree], v: String} -> DocTree
con Leaf : use TokenReader in Token -> DocTree


lang BreakerChooserInterface

    syn State =
        | Program {}
        | Let {}

    sem change /- (State, String) -> State -/ =
    sem choose /- (State, String) -> [String] -/ =
    
end

-- TODO: let, lang, con, type, use
    
lang ProgramBreakerChooser = BreakerChooserInterface
    
    sem change =
        | (Program {}, "let") -> Let {}
        | (Program {}, "lang") -> never
        | (Program {}, "type") -> never

    sem choose =
        | (Program {}, "let") -> ["lang", "let", "type"]
        | (Program {}, "lang") -> ["end"]
        | (Program {}, "type") -> ["let", "type", "lang"]
end

lang LetBreakerChooser = BreakerChooserInterface

    sem change = 
        | (Let {}, "let") -> Let {}
        | (Let {}, "type") -> never
    
    sem choose =
        | (Let {}, "let") -> ["in"]
        | (Let {}, "type") -> ["in"]
end

    
lang BreakerChooser = ProgramBreakerChooser + LetBreakerChooser end

let displayTree : (DocTree -> ()) = use TokenReader in lam tree.
    recursive
    let displayTree = lam tree.
        match tree with Node { sons = sons, v = v } then
            print v;
            print "\n--------------\n";
            iter displayTree sons 
        else match tree with Leaf token in
            print (lit token);
            print "\n--------------\n"
    in displayTree tree
          

let parse = use TokenReader in use BreakerChooser in lam txt.
 let headSnippets = ["let", "lang"] in

 recursive   
 let parseRec: ([Char] -> [[String]] -> { tree: [DocTree], stream: [Char] }) =
    lam txt. lam breakers.

        let buildSnippet = lam word. lam breakers.
            let lword = lit word.token in
            let snippet = parseRec word.stream (cons (choose (Program {}, lword)) breakers) in
            let docNode = Node { sons = snippet.tree, v = lword } in
            let output = parseRec snippet.stream breakers in
            { output with  tree = cons docNode output.tree }
        in

        recursive
        let contains = lam arr. lam lword. 
            match arr with [] then
                false
            else
                or (eqString (head arr) lword) (contains (tail arr) lword)
        in

        let word = next txt in
        let lword = lit word.token in
        if contains (head breakers) lword then
            { tree = [Leaf word.token], stream = word.stream }
        else match word.token with Eof {} then
            { tree = [Leaf word.token], stream = "" }
        else if contains headSnippets lword then
            buildSnippet word breakers
        else
            let output = parseRec word.stream breakers in
            { output with tree = cons (Leaf word.token) output.tree }
        in
    let output = parseRec txt [[""]] in
    Node { sons = output.tree, v = "program" }
