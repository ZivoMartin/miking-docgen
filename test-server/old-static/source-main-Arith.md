```ocaml
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
```
