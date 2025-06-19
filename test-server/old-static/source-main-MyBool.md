```
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
```
