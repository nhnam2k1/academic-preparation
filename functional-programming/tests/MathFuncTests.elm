module MathFuncTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import MathFunc exposing (..)

printSuite : Test
printSuite = describe "print" 
    [
        test "expect Const 2 is 2" 
        (\_ -> print (Const 2)
            |> Expect.equal "2"),
        test "expect X is X" 
        (\_ -> print (X)
            |> Expect.equal "x"),
        test "expect Mult (Const 2) X  is (2 * x)" 
        (\_ -> print (Mult (Const 2) X)
            |> Expect.equal "(2 * x)"),
        test "expect Div (Const 2) X  is (2 / x)" 
        (\_ -> print (Div (Const 2) X)
            |> Expect.equal "(2 / x)"),
        test "expect Plus (Const 2) X  is (2 + x)" 
        (\_ -> print (Plus (Const 2) X)
            |> Expect.equal "(2 + x)"),
        test "expect Minus (Const 2) X  is (2 - x)" 
        (\_ -> print (Minus (Const 2) X)
            |> Expect.equal "(2 - x)"),
        test "Sample test" 
        (\_ -> print (Plus (Mult (Plus (Const 3) X) (Minus X (Poly X 5))) (Const 2))
            |> Expect.equal "(((3 + x) * (x - (x ^ 5))) + 2)")
    ]

evalSuite : Test
evalSuite = describe "eval" 
    [
        test "expect replace x by 5 in Const 2 is 2" 
        (\_ -> eval 5 (Const 2)
            |> Expect.equal 2),
        test "expect replace x by 5 in X is 5" 
        (\_ -> eval 5 (X)
            |> Expect.equal 5),
        test "expect replace x by 5 in (2 * x) is 10" 
        (\_ -> eval 5 (Mult (Const 2) X)
            |> Expect.equal 10),
        test "expect replace x by 5 in (2 / x) is 0.4" 
        (\_ -> eval 5 (Div (Const 2) X)
            |> Expect.within (Expect.Absolute 0.000000001) 0.4),
        test "expect replace x by 5 in (2 + x) is 7" 
        (\_ -> eval 5 (Plus (Const 2) X)
            |> Expect.equal 7),
        test "expect replace x by 5 in (2 - x) is -3" 
        (\_ -> eval 5 (Minus (Const 2) X)
            |> Expect.equal -3),
        test "Sample test" 
        (\_ -> eval 2 (Plus (Mult (Plus (Const 3) X) (Minus X (Poly X 5))) (Const 2))
            |> Expect.equal -148)
    ]