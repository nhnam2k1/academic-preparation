module MathFunc2Test exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import MathFunc2 exposing (..)
import MathFunc exposing (Function(..))
import MathFunc exposing (print)

derivativeSuite : Test
derivativeSuite = describe "derivative"
    [
        test "(10)' = 0" 
        (\_ -> derivative (Const 10) |> print
            |> Expect.equal "0"),

        test "(X)' = 1" 
        (\_ -> derivative (X) |> print
            |> Expect.equal "1"),
        
        test "(x + 5)' = 0 + 1 = 1" 
        (\_ -> derivative (Plus X (Const 5)) |> print
            |> Expect.equal "(0 + 1)"),
        
        test "(5 - x)' = 0 - 1 = -1 + 0" 
        (\_ -> derivative (Minus (Const 5) X) |> print
            |> Expect.equal "(-1 + 0)"),

        test "(5*X)' = 5'X + X'5 = 0 + 5= 5" 
        (\_ -> derivative (Mult (Const 5) X) |> print
            |> Expect.equal "((0 * x) + (1 * 5))"),
        
        test "(5 / x)' = (5'x - x'5) / x^2 = (0 - 5) / x^2" 
        (\_ -> derivative (Div (Const 5) X) |> print
            |> Expect.equal "(((0 * x) - (1 * 5)) / (x ^ 2))"),

        test "(x + 5)^5 = 5(x+5)'(x+5)^4 = 5*1(x+5)^4" 
        (\_ -> derivative (Poly (Plus X (Const 5)) 5) |> print
            |> Expect.equal "(5 * ((0 + 1) * ((x + 5) ^ 4)))")
    ]

simplifySuite : Test
simplifySuite = describe "simplify"
    [
        test "5 + 5 = 10" 
        (\_ -> simplify (Plus (Const 5) (Const 5)) |> print
            |> Expect.equal "10"),

        test "0 + x + x = x + x" 
        (\_ -> simplify (Plus (Const 0) (Plus X X)) |> print
            |> Expect.equal "(x + x)"),
        
        test "x + x + 0 = x + x" 
        (\_ -> simplify (Plus (Plus X X) (Const 0)) |> print
            |> Expect.equal "(x + x)"),

        test "(5 - x) + 5 = 10 - x" 
        (\_ -> simplify (Plus (Minus (Const 5) X) (Const 5)) 
            |> print |> Expect.equal "(10 - x)"),

        test "(5 + x) + 5 = 10 + x" 
        (\_ -> simplify (Plus (Plus (Const 5) X) (Const 5)) 
            |> print |> Expect.equal "(10 + x)"),

        test "(5 - x) + (5 - x) = 10 - x - x = 10 - (x + x)" 
        (\_ -> simplify (Plus (Minus (Const 5) X) (Minus (Const 5) X))
            |> print |> Expect.equal "(10 - (x + x))"),

        test "(x + 5)^5 = 5(x+5)'(x+5)^4 = 5*(x+5)^4" 
        (\_ -> derivative (Poly (Plus X (Const 5)) 5) 
            |> simplify |> print
            |> Expect.equal "(5 * ((x + 5) ^ 4))"),

        test "Sample test" 
        (\_ -> derivative (Plus (Mult (Plus (Const 3) X) (Minus X (Poly X 5))) (Const 2)) 
            |> simplify |> print
            |> Expect.equal "((x - (x ^ 5)) + ((1 - (5 * (x ^ 4))) * (3 + x)))")
    ]

-- I wish I had other teammates for testing