-- Lire de mot en mot, un mot est:
--   - Une suite de lettre entre guillements
--   - Une suite de lettre entre '--' et un retour à la ligne
--   - N'importe quelle autre suite de lettre comprises entre deux seperateurs
    
    

include "token-readers.mc"
         

mexpr
    use TokenReader in
    recursive
    let parse = 
        lam txt.
        match txt with "" then
            ()
        else
            let new = next txt in
            display new.token;
            print "\n";
            parse new.stream
    in
    let txt = "\"test\"--hh
/-hey-/okj sozks--test" in
    parse txt;
    print "\n"

