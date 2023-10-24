module PythagorasP2Test exposing (..)

import PythagorasP2 exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)

pythTriplesMapSuite : Test
pythTriplesMapSuite = describe "pythTriplesMap"
    [
        test "expect" 
        (\_ -> pythTriplesMap [(5,4),(2,1),(35,7)]
            |> Expect.equal [(9,40,41),(3,4,5),(1176,490,1274)])
    ]

pythTriplesRecSuite : Test
pythTriplesRecSuite = describe "pythTriplesRec"
    [
        test "expect" 
        (\_ -> pythTriplesRec [(5,4),(2,1),(35,7)]
            |> Expect.equal [(9,40,41),(3,4,5),(1176,490,1274)])
    ]

arePythTriplesRecSuite : Test
arePythTriplesRecSuite = describe "arePythTriplesRec"
    [
        test "expect" 
        (\_ -> arePythTriplesRec [(1,2,3), (9,40,41), (3,4,5), (100,2,500)]
            |> Expect.equal [(9,40,41),(3,4,5)])
    ]

arePythTriplesFilterSuite : Test
arePythTriplesFilterSuite = describe "arePythTriplesFilter"
    [
        test "expect" 
        (\_ -> arePythTriplesFilter [(1,2,3), (9,40,41), (3,4,5), (100,2,500)]
            |> Expect.equal [(9,40,41),(3,4,5)])
    ]