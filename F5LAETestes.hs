module F5LAETestes where

import Test.HUnit

import F5LAE

eval2 :: Exp -> Type 
eval2 exp = [] |- exp


-- tc01 = TestCase (assertEqual "num 3" ((eval2 (Num 3)) (TInt)))

tc02 = TestCase (assertEqual "for let (x,int) = Add (4,0) in let (y,bool) = 5 in y"
                             (TInt)
                             (eval2 (Let ("x", TBool) (Add (Num 4) (Num 0)) (Let ("y",TInt) (Num 5) (Ref "y")))))

tc03 = TestCase (assertEqual "for let (x, bool) = Add (4,0) in x + 5"
                             (TError)
                             (eval2 (Let ("x", TBool) (Add (Num 4) (Num 0)) (Add (Num 5) (Ref "x")))))

tc04 = TestCase (assertEqual "for let (x,int) = Add (4,0) in let (y, bool) = 5 in y"
                             (TError)
                             (eval2 (Let ("x", TBool) (Add (Num 4) (Num 0)) (Let ("y",TBool) (Num 5) (Ref "y")))))

tc05 = TestCase (assertEqual "if0 (3) then 2 else 1"
                             (TInt)
                             (eval2 (IF0 (Num 3) (Num 2) (Num 1))))

tc06 = TestCase (assertEqual "if0 (true) then 2 else 1"
                             (TError)
                             (eval2 (IF0 (Bool True) (Num 2) (Num 1))))

tc07 = TestCase (assertEqual "if0 (true) then 2 else 1"
                             (TError)
                             (eval2 (IF0 (Num 0) (Bool False) (Num 1))))

allTCs = TestList $ map (\tc -> TestLabel "test" tc) [tc02, tc03, tc04, tc05, tc06, tc07] 