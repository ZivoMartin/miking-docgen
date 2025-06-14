-- This code defines two small languages:
--  Arith: supports integers and addition, with evaluation rules for both.
--  MyBool: supports booleans and conditionals, with evaluation based on boolean values.


    
-- Definition of a simple arithmetic language
lang Arith
    
  -- Definition of the syntax for expressions
  syn Expr =
  | Num Int             
  | Add (Expr, Expr)    

  -- Definition of the evaluation semantics
  sem eval =
  | Num n -> Num n      
  | Add (e1,e2) ->      
    match eval e1 with Num n1 then                 
      match eval e2 with Num n2 then               
        Num (addi n1 n2)                           
      else error "Not a number"                    
    else error "Not a number"                      
end

-- Definition of a boolean language
lang MyBool
  -- Syntax of boolean expressions
  syn Expr =
  | True()                  
  | False()                 
  | If (Expr, Expr, Expr)   

  -- Evaluation semantics
  sem eval =
  | True() -> True()        
  | False() -> False()      
  | If(cnd,thn,els) ->      
    let cndVal = eval cnd in  -- Storing the evaluation result
    match cndVal with True() then eval thn        
    else match cndVal with False() then eval els  
    else error "Not a boolean"                    
end


-- Main expression to run or test
mexpr ()
