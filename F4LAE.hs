module F4LAE where 

import Prelude hiding (lookup)

-- | Several type synonymous to improve readability 
type Name = String  
type FormalArg = String 
type Id = String 

-- | A type for representing function declaration
data FunDec = FunDec Name FormalArg Exp 
     deriving(Show, Eq)

-- | The abstract syntax of F4LAE  
data Exp = Num Integer
         | Add Exp Exp 
         | Sub Exp Exp 
         | Let Id Exp Exp
         | Ref Id 
         | App Name Exp 
         | Lambda FormalArg Exp
         | LambdaApp Exp Exp
         | IF0 Exp Exp Exp 
     deriving(Show, Eq)     

-- | The environment with the list of deferred substitutions. 
type DefrdSub = [(Id, Value)] 


-- | The value data type.
-- 
-- In this language (F4LAE), an expression
-- is reduced to a value: either a number or
-- a closure. A closure is a lambda expression
-- that *closes* together its definition with the
-- pending substitutions at definition time. 
--
data Value = NumValue Integer
           | Closure FormalArg Exp DefrdSub
           | ExpV Exp DefrdSub
     deriving(Show, Eq)

-- | The interpreter function \0/
interp :: Exp -> DefrdSub -> [FunDec] -> Value

-- the simple interpreter for numbers. 
interp (Num n) ds decs = NumValue n

-- the interpreter for an add expression.                            
interp (Add e1 e2) ds decs = NumValue (v1 + v2) 
  where
    (NumValue v1, ds2) = strict (interp e1 ds decs) ds decs 
    NumValue v2 = fst(strict (interp e2 ds2 decs) ds decs)

-- the interpreter for a sub expression. 
interp (Sub e1 e2) ds decs = NumValue (v1 - v2) 
  where
    (NumValue v1, ds2) = strict (interp e1 ds decs) ds decs
    NumValue v2 = fst(strict (interp e2 ds2 decs) ds decs)

-- the interpreter for a let expression.
-- note here that, to improve reuse, we actually
-- convert a let exprssion in the equivalent
-- lambda application (ela), and then interpret it.  
interp (Let v e1 e2) ds decs = interp ela ds decs
  where ela = (LambdaApp (Lambda v e2) e1) 

-- the interpreter for a reference to a variable.
-- here, we make a lookup for the reference, in the
-- list of deferred substitutions. 
interp (Ref v) ds decs =
  let res = lookup v fst ds
  in case res of
    (Nothing) -> error $ "variable " ++ v ++ " not found"
    (Just (_, value)) -> value 

-- the interpreter for a function application.
-- here, we first make a lookup for the function declaration,
-- evaluates the actual argument (leading to the parameter pmt),  
-- and then we interpret the function body in a new "local" environment
-- (env). 
interp (App n e) ds decs =
  let res = lookup n (\(FunDec n _ _) -> n) decs
  in case res of
    (Nothing) -> error $ "funtion " ++ n ++ " not found"
    (Just (FunDec _ farg body)) -> interp body env decs
     where
       expV = ExpV e ds
       env = [(farg, expV)] 

-- the interpreter for a lambda abstraction.
-- that is the most interesting case (IMO). it
-- just returns a closure!
interp (Lambda farg body) ds decs = Closure farg body ds

-- the interpreter for a lambda application.
-- plese, infer what is going on here in this
-- case
interp (LambdaApp e1 e2) ds decs = interp body env decs -- we interpret considering the new environment
  where
    Closure farg body ds0 = fst(strict (interp e1 ds decs) ds decs)  -- we expect e1 to evaluate to a closure.
    expV = ExpV e2 ds                   -- ds0 is the deferred substitutions at the lambda declaration
    env  = (farg,expV):ds0              -- env is the original environment (ds0) + a new mapping

strict :: Value -> DefrdSub -> [FunDec] -> (Value, DefrdSub)
strict n@(NumValue v) env _ = (n,env)

strict c@(Closure farg body ds) env _ = (c,env)

strict e@(ExpV e1 ds) env decs =  strict  val env2 decs 
  where
    val = interp e1 ds decs
    env2 = search e val env

search :: Value -> Value -> DefrdSub -> DefrdSub
search _ _ [] = []
search expV newV ((id, val):xs)  
 | val == expV = (id, newV) : search expV newV xs
 | otherwise   = (id, val)  : search expV newV xs

-- a new lookup function.   
lookup :: Id -> (a -> String) -> [a] -> Maybe a
lookup _ f [] = Nothing
lookup v f (x:xs)  
 | v == f x = Just x
 | otherwise = lookup v f xs

