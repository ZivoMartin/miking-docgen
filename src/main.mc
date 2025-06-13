include "parser.mc"



mexpr
    use TokenReader in
    let txt = "
-- Hello, World
let x =
    let y = 8 
        let z = 3 in
        z * 2 in
    addi y 4
let w = let x = 7 in 2
let a = let b = 7 in 2
let y = 3 in
lang Arith
end 
    " in
    let result = parse txt in
    displayTree result

    
