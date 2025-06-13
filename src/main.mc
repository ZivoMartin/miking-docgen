include "parser.mc"



mexpr
    use TokenReader in
    let txt = "
let x =
    let y = 8 in
    addi y 3
let w = 7
lang Arith
end
    " in
    let result = parse txt in
    displayTree result

