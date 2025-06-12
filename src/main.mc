-- Lire de mot en mot, un mot est:
--   - Une suite de lettre entre guillements
--   - Une suite de lettre entre '--' et un retour à la ligne
--   - N'importe quelle autre suite de lettre comprises entre deux seperateurs
    
    

include "parser.mc"



mexpr
    use TokenReader in
    let txt = "\"test\"/-hey-/okj let sozks in let --test \nlet in izejdu ino" in    
    let result = parse txt in
    displayTree result

