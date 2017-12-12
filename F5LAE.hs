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
data FunDec = FunDec Name Args Exp 
     deriving(Show, Eq)

-- | The abstract syntax of F4LAE  
data Exp = Num Integer
         | Bool Bool
         | Add Exp Exp 
         | Sub Exp Exp
         | Div Exp Exp
         | And Exp Exp | Or Exp Exp | Not Exp 
         | Let Id Exp Exp
         | Ref Id 
         | App Name Exps 
         | Lambda (FormalArg, Type) Exp
         | LambdaApp Exp Exp
         | IF0 Exp Exp Exp 
         | IF Exp Exp Exp 
     deriving(Show, Eq)     

-- | The environment with the list of deferred substitutions. 
type DefrdSub = [(Id, Value)] 
type Gamma = [(Id,Type)]
type Args = [(Id,Type)]
type Exps = [Exp]

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

interp (Let v e1 e2) ds decs = interp e2 ds2 decs
  where
    i1 = ExpV e1 ds
    ds2 = (v,i1):ds

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
-- interp (App n e) ds decs =
--   let res = lookup n (\(FunDec n _ _) -> n) decs
--   in case res of
--     (Nothing) -> error $ "funtion " ++ n ++ " not found"
--     (Just (FunDec _ (farg, _) body)) -> interp body env decs
--      where
--        expV = ExpV e ds
--        env = [(farg, expV)]

interp (App n exps) ds decs =
  let res = lookup n (\(FunDec n _ _) -> n) decs
  in case res of
    (Nothing) -> error $ "funtion " ++ n ++ " not found"
    (Just (FunDec _ args body)) -> interp body ((envWithArgs args exps ds) ++ ds) decs

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

envWithArgs :: Args -> Exps -> DefrdSub -> DefrdSub
envWithArgs [] _ _ = []
envWithArgs _ [] _ = []
envWithArgs ((id,_):as)  (exp:es) ds = (id,expV):cont
  where
    expV = ExpV exp ds
    cont = envWithArgs as es ds


(|-) :: (Gamma,[FunDec]) -> Exp -> Type
(|-) _ (Bool b) = TBool 
(|-) _ (Num n)  = TInt
(|-) (g,_) (Ref v)  = 
  let res = lookup v fst g
  in case res of
    (Nothing) -> error $ "variable " ++ v ++ " not found"
    (Just (_, value)) -> value

(|-) gamma (Add l r)    = if (((gamma |- l) == TInt) && (gamma |- r) == TInt) then  TInt else TError
(|-) gamma (Sub l r)    = if (((gamma |- l) == TInt) && (gamma |- r) == TInt) then  TInt else TError
(|-) gamma (Div l r)    = if (((gamma |- l) == TInt) && (gamma |- r) == TInt) then  TInt else TError
(|-) gamma (IF0 c t e)  = if ((tcond == TInt) && (telse == tthen)) then tthen else TError
  where
    tcond = gamma |- c
    tthen = gamma |- t
    telse = gamma |- e

(|-) gamma (IF c t e)  = if ((tcond == TBool) && (telse == tthen)) then tthen else TError
  where
    tcond = gamma |- c
    tthen = gamma |- t
    telse = gamma |- e

(|-) gamma@(g,f) (Let x e c)  = if t1 == TError then TError else t2
  where
    t1 = gamma |- e
    t2 = (((x, t1) : g),f) |- c

(|-) (g,f) (Lambda (fa, t1) e) = TFunc t1 t2
  where
    t2 = (((fa,t1) : g ),f)|- e

(|-) gamma (LambdaApp e1 e2) = if ta == t1 then t2 else TError
  where
    (TFunc t1 t2) = gamma |- e1
    ta = gamma |- e2 

(|-) gamma (And e1 e2) = if ((te1 == TBool) && (te2 == TBool)) then TBool else TError 
  where
    te1 = gamma |- e1
    te2 = gamma |- e2

(|-) gamma (Or e1 e2) = if ((te1 == TBool) && (te2 == TBool)) then TBool else TError 
  where
    te1 = gamma |- e1
    te2 = gamma |- e2

(|-) gamma (Not e) = if (te == TBool) then TBool else TError
  where
    te = gamma |- e

(|-) gamma@(g,decs) (App n exps) = 
    let res = lookup n (\(FunDec n _ _) -> n) decs
    in case res of
      (Nothing) -> error $ "funtion " ++ n ++ " not found"
      (Just (FunDec _ args body)) -> let res2 = tcArgs gamma args exps
                                     in case res2 of
                                     (Just g2) -> ((g2 ++ g),decs) |- body 
                                     (Nothing) -> TError 


tcArgs :: (Gamma,[FunDec]) -> Args -> Exps -> Maybe Gamma
tcArgs _ [] _ = Just []
tcArgs _ _ [] = Just []
tcArgs gamma ((id,t):as) (e:es) = 
  let res = tcArgs gamma as es
  in case res of
    (Just g) -> if t == (gamma |- e) then (Just ((id,t):g)) else Nothing
    (Nothing) -> Nothing







