module F5LAETestes where

import Test.HUnit

import F5LAE

eval2 :: Exp -> Type 
eval2 exp = [] |- exp


tc01 = TestCase (assertEqual "Tipo Int" (eval2 (Num 3)) (TInt))

