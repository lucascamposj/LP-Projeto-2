module F4LAETestes where

import Test.HUnit

import F4LAE

eval2 :: Exp -> Value
eval2 exp = interp exp [] []


tc01 = TestCase (assertEqual "for eval 4" (eval2 (Num 4)) (NumValue 4))

tc02 = TestCase (assertEqual "for let x = 3 in x" 
                             (ExpV (Num 3) [])
                             (eval2 (Let "x" (Num 3) (Ref "x"))))

tc03 = TestCase (assertEqual "for let x = 3 in x + x"
                             (NumValue 6)
                             (eval2 (Let "x" (Num 3) (Add (Ref "x") (Ref "x")))))

tc04 = TestCase (assertEqual "for let x = Add (4,0) in 5"
                             (NumValue 5)
                             (eval2 (Let "x" (Add (Num 4) (Num 0)) (Num 5))))


tc05 = TestCase (assertEqual "for let x = Add (4,0) in let y = 5 in y"
                             (ExpV (Num 5) [("x",ExpV (Add (Num 4) (Num 0)) [])])
                             (eval2 (Let "x" (Add (Num 4) (Num 0)) (Let "y" (Num 5) (Ref "y")))))

tc06 = TestCase (assertEqual "for let x = 10 in let f = y -> x + y in let x = 5 in f (x + 3)"
                              (NumValue 18) 
                              (eval2 (Let "x" (Num 10)
                                             (Let "f" (Lambda "y" (Add (Ref "x") (Ref "y")))
                                                      (Let "x" (Num 5) (LambdaApp (Ref "f")
                                                                        (Add (Ref "x") (Num 3))))))))

tc07 = TestCase (assertEqual "for let x = Add (10,5) in Let y = Add(Ref('x'),20) in Let x = 5 in Add(Ref('y'),Ref('x')) "
                             (NumValue 40)
                             (eval2 (Let "x" (Add (Num 10) (Num 5)) 
                                            (Let "y"  (Add (Ref("x")) (Num 20)) 
                                                      (Let "x" (Num 5) (Add (Ref("y")) (Ref("x")) ))))))

allTCs = TestList $ map (\tc -> TestLabel "test" tc) [tc01, tc02, tc03, tc04,tc05,tc06,tc07]       
