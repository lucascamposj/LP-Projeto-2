module F5LAE where 

import Prelude hiding (lookup)

-- | Several type synonymous to improve readability 
type Name = String  
type FormalArg = String 
type Id = String 

-- | A representation of the types of our language
data Type = TInt | TBool | TFunc Type Type | TError
 deriving(Show, Eq) 

-- | A type for representing function declaration
data FunDec = FunDec Name FormalArg Exp 
     deriving(Show, Eq)

-- | The abstract syntax of F4LAE  
data Exp = Num Integer
         | Bool Bool
         | Add Exp Exp 
         | Sub Exp Exp
         | Div Exp Exp
         | And Exp Exp | Or Exp Exp | Not Exp 
         | Let (Id,Type) Exp Exp
         | Ref Id 
         | App Name Exp 
         | Lambda (FormalArg, Type) Exp
         | LambdaApp Exp Exp
         | IF0 Exp Exp Exp 
         | IF Exp Exp Exp 
     deriving(Show, Eq)     

-- | The environment with the list of deferred substitutions. 
type DefrdSub = [(Id, Value)] 
type Gamma = [(Id,Type)]

-- | The value data type.
-- 
-- In this language (F4LAE), an expression
-- is reduced to a value: either a number or
-- a closure. A closure is a lambda expression
-- that *closes* together its definition with the
-- pending substitutions at definition time. 
--
data Value = NumValue Integer
           | BoolValue Bool
           | Closure (FormalArg, Type) Exp DefrdSub
           | ExpV Exp DefrdSub
     deriving(Show, Eq)

-- | The interpreter function \0/
interp :: Exp -> DefrdSub -> [FunDec] -> Value

-- the simple interpreter for numbers. 
interp (Num n) ds decs = NumValue n

-- the interpreter for Bool 
interp (Bool n) ds decs = BoolValue n 

-- the interpreter for an add expression.                            
interp (Add e1 e2) ds decs = NumValue (v1 + v2) 
  where
    NumValue v1 = strict (interp e1 ds decs) decs 
    NumValue v2 = strict (interp e2 ds decs) decs 

-- the interpreter for a sub expression. 
interp (Sub e1 e2) ds decs = NumValue (v1 - v2) 
  where
    NumValue v1 = strict (interp e1 ds decs) decs
    NumValue v2 = strict (interp e2 ds decs) decs

interp (Div e1 e2) ds decs = NumValue (v1 `div` v2)
 where
   NumValue v1 = strict (interp e1 ds decs) decs
   NumValue v2 = strict (interp e2 ds decs) decs
   
-- the interpreter for a let expression.
-- note here that, to improve reuse, we actually
-- convert a let exprssion in the equivalent
-- lambda application (ela), and then interpret it.  

--interp (Let () e1 e2) ds decs = interp ela ds decs 
  --where ela = (LambdaApp (Lambda () e2) e1) 

interp (Let (v,t) e1 e2) ds decs = interp ela ds decs
  where
    ela = (LambdaApp (Lambda (v,t) e2) e1)

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
interp (Lambda (farg, t) body) ds decs = Closure (farg, t) body ds

-- the interpreter for a lambda application.
-- plese, infer what is going on here in this
-- case
interp (LambdaApp e1 e2) ds decs = interp body env decs 
  where
    Closure (farg, t) body ds0 = strict (interp e1 ds decs) decs  
    expV = ExpV e2 ds     
    env  = (farg,expV):ds0

interp (IF0 cond true false) subs decs
  | (interp cond subs decs) == (NumValue 0) = interp true subs decs
  | otherwise = interp false subs decs

interp (IF cond true false) subs decs
  | (interp cond subs decs) == (BoolValue True) = interp true subs decs
  | otherwise = interp false subs decs

interp (And e1 e2) subs decs = BoolValue (i1 && i2) 
  where 
    BoolValue i1 = strict (interp e1 subs decs) decs
    BoolValue i2 = strict (interp e2 subs decs) decs

interp (Or e1 e2) subs decs = BoolValue (i1 || i2) 
  where 
    BoolValue i1 = strict (interp e1 subs decs) decs
    BoolValue i2 = strict (interp e2 subs decs) decs    

interp (Not e) subs decs = BoolValue (not(i)) 
  where 
    BoolValue i = strict (interp e subs decs) decs
   

strict :: Value -> [FunDec] -> Value
strict n@(NumValue v) _ = n
strict c@(Closure farg body ds) _ = c
strict b@(BoolValue v) _ = b 
strict (ExpV e1 ds) decs = strict (interp e1 ds decs) decs

-- a new lookup function.   
lookup :: Id -> (a -> String) -> [a] -> Maybe a
lookup _ f [] = Nothing
lookup v f (x:xs)  
 | v == f x = Just x
 | otherwise = lookup v f xs


(|-) :: Gamma -> Exp -> Type
(|-) _ (Bool b) = TBool 
(|-) _ (Num n)  = TInt
(|-) g (Ref v)  = 
  let res = lookup v fst g
  in case res of
    (Nothing) -> error $ "variable " ++ v ++ " not found"
    (Just (_, value)) -> value

(|-) g (Add l r)    = if (((g |- l) == TInt) && (g |- r) == TInt) then  TInt else TError
(|-) g (Sub l r)    = if (((g |- l) == TInt) && (g |- r) == TInt) then  TInt else TError
(|-) g (Div l r)    = if (((g |- l) == TInt) && (g |- r) == TInt) then  TInt else TError
(|-) g (IF0 c t e)  = if ((tcond == TInt) && (telse == tthen)) then tthen else TError
  where
    tcond = g |- c
    tthen = g |- t
    telse = g |- e

(|-) g (IF c t e)  = if ((tcond == TBool) && (telse == tthen)) then tthen else TError
  where
    tcond = g |- c
    tthen = g |- t
    telse = g |- e

(|-) g (Let x e c)  = t2
  where
    t1 = g |- e
    t2 = ((x, t1) : g) |- c
(|-) g (Lambda (fa, t1) e) = TFunc t1 t2
  where
    t2 = ((fa,t1) : g ) |- e
(|-) g (LambdaApp e1 e2) = if ta == t1 then t2 else TError
  where
    (TFunc t1 t2) = g |- e1
    ta = g |- e2 

(|-) g (And e1 e2) = if ((te1 == TBool) && (te2 == TBool)) then TBool else TError 
  where
    te1 = g |- e1
    te2 = g |- e2
(|-) g (Or e1 e2) = if ((te1 == TBool) && (te2 == TBool)) then TBool else TError 
  where
    te1 = g |- e1
    te2 = g |- e2
(|-) g (Not e) = if (te == TBool) then TBool else TError
  where
    te = g |- e


