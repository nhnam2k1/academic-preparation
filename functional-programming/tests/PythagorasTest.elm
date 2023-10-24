module PythagorasTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Pythagoras exposing(..)

sqrSuite : Test
sqrSuite = describe "sqr function"
    [
        test "expect 5^2 = 25" 
        (\_ -> sqr 5 
            |> Expect.equal 25),
        test "expect 7^2 = 49" 
        (\_ -> sqr 7 
            |> Expect.equal 49),
        test "expect(-7)^2 = 49" 
        (\_ -> sqr -7
            |> Expect.equal 49)
    ]

isTripleSuite : Test
isTripleSuite = describe "isTriple"
    [
        test "expect (3,4,5) is a Pythagorean triple" 
        (\_ -> isTriple 3 4 5 
            |> Expect.equal True),
        test "expect (3,4,6) is not a Pythagorean triple" 
        (\_ -> isTriple 3 4 6
            |> Expect.equal False),
        test "expect (-3,4,5) is not a Pythagorean triple because of negative length" 
        (\_ -> isTriple -3 4 5
            |> Expect.equal False),
        test "expect (3,-4,5) is not a Pythagorean triple because of negative length" 
        (\_ -> isTriple 3 -4 5
            |> Expect.equal False),
        test "expect (3,4,-5) is not a Pythagorean triple because of negative length" 
        (\_ -> isTriple 3 4 -5
            |> Expect.equal False),
        test "expect (0,4,5) is not a Pythagorean triple because of zero length" 
        (\_ -> isTriple 0 4 5
            |> Expect.equal False),
        test "expect (3,0,5) is not a Pythagorean triple because of zero length" 
        (\_ -> isTriple 3 0 5
            |> Expect.equal False),
        test "expect (3,4,0) is not a Pythagorean triple because of zero length" 
        (\_ -> isTriple 3 4 0
            |> Expect.equal False)
    ]

pythTripleSuite : Test
pythTripleSuite = describe "pythTriple"
    [
        test "expect (5, 4) generate leg 1 of length 9" 
        (\_ -> leg1 5 4 
            |> Expect.equal 9),
        test "expect (5, 4) generate leg 2 of length 40" 
        (\_ -> leg2 5 4 
            |> Expect.equal 40),
        test "expect (5, 4) generate hyp of length 41" 
        (\_ -> hyp 5 4 
            |> Expect.equal 41),
        test "expect (9, 40, 41) is a Pythagorean triple" 
        (\_ -> isTriple 9 40 41
            |> Expect.equal True),
        test "expect (5, 4) generate Pythagorean triple (9, 40, 41)" 
        (\_ -> pythTriple (5, 4)
            |> Expect.equal (9, 40, 41)),
        test "expect (0, 5) generate (0, 0, 0) because of zero integer" 
        (\_ -> pythTriple (0, 5)
            |> Expect.equal (0, 0, 0)),
        test "expect (-5, 5) generate (0, 0, 0) because of negative integer" 
        (\_ -> pythTriple (-5, 5) 
            |> Expect.equal (0, 0, 0)),
        test "expect (1, 5) generate (0, 0, 0) because of 1 < 5" 
        (\_ -> pythTriple (1, 5)
            |> Expect.equal (0, 0, 0))
    ]

isTripleTupleSuite : Test
isTripleTupleSuite = describe "isTripleTuple"
    [
        test "expect (3,4,5) is a Pythagorean triple" 
        (\_ -> isTripleTuple (3,4,5)
            |> Expect.equal True),
        test "expect (9,40,41) is a Pythagorean triple" 
        (\_ -> isTripleTuple (9,40,41)
            |> Expect.equal True),
        test "expect result from pythTriple (5,4) is a Pythagorean triple" 
        (\_ -> isTripleTuple (pythTriple (5,4))
            |> Expect.equal True),
        test "expect (3,4,6) is not a Pythagorean triple" 
        (\_ -> isTripleTuple (3,4,6)
            |> Expect.equal False),
        test "expect (-3,4,5) is not a Pythagorean triple because of negative length" 
        (\_ -> isTripleTuple (-3,4,5)
            |> Expect.equal False),
        test "expect (3,-4,5) is not a Pythagorean triple because of negative length" 
        (\_ -> isTripleTuple (3,-4,5)
            |> Expect.equal False),
        test "expect (3,4,-5) is not a Pythagorean triple because of negative length" 
        (\_ -> isTripleTuple (3,4,-5)
            |> Expect.equal False),
        test "expect (0,4,5) is not a Pythagorean triple because of zero length" 
        (\_ -> isTripleTuple (0,4,5)
            |> Expect.equal False),
        test "expect (3,0,5) is not a Pythagorean triple because of zero length" 
        (\_ -> isTripleTuple (3,0,5)
            |> Expect.equal False),
        test "expect (3,4,0) is not a Pythagorean triple because of zero length" 
        (\_ -> isTripleTuple (3,4,0)
            |> Expect.equal False)
    ]