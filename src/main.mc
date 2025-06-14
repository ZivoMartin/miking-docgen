include "parser.mc"

mexpr
    use TokenReader in
    let txt1 = "

include \"Hello\"
-- Hello, World
recursive
let x =
    recursive
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
   let txt2 = "
-- This code defines two small languages:
--  Arith: supports integers and addition, with evaluation rules for both.
--  MyBool: supports booleans and conditionals, with evaluation based on boolean values.

include \"Hello\"
    
-- Definition of a simple arithmetic language
lang Arith

  type T = Int
  
  -- Definition of the syntax for expressions
  syn Expr =
  | Num Int             
  | Add (Expr, Expr)    

  -- Definition of the evaluation semantics
  sem eval =
    con G in
  type T = Int in 5
  type T = Int
  con G
  type T = Int
    con G
end



-- Main expression to run or test
mexpr ()" in
    let result = parse txt1 in
    displayTree result

    
