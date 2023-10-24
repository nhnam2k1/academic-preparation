module CaesarTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Caesar exposing(..)

encodeSuite : Test
encodeSuite = describe "encode"
    [
        test "expect x by offset 5 to c" 
        (\_ -> encode 5 'x' 
            |> Expect.equal 'c'),
        test "check 28 mod 26 = 2" 
        (\_ -> modBy 26 28 
            |> Expect.equal 2),
        test "expect T by offset 7 to A" 
        (\_ -> encode 7 'T' 
            |> Expect.equal 'A'),
        test "expect _ by offset 7 to _" 
        (\_ -> encode 7 ' ' 
            |> Expect.equal ' '),
        test "expect x by offset -1 will not change because of offset is negative" 
        (\_ -> encode -1 'x'
            |> Expect.equal 'x'),
        test "expect x by offset 27 will not change because of offset is greater 26" 
        (\_ -> encode 27 'x'
            |> Expect.equal 'x'),
        test "expect x by offset 0 will not change" 
        (\_ -> encode 0 'x'
            |> Expect.equal 'x')
    ]

decodeSuite : Test
decodeSuite = describe "decode"
    [
        test "expect c by offset 5 to x" 
        (\_ -> decode 5 'c' 
            |> Expect.equal 'x'),
        test "expect A by offset 7 to T" 
        (\_ -> decode 7 'A' 
            |> Expect.equal 'T'),
        test "expect _ by offset 7 to _" 
        (\_ -> decode 7 ' ' 
            |> Expect.equal ' '),
        test "expect x by offset -1 will not change because of offset is negative" 
        (\_ -> decode -1 'x'
            |> Expect.equal 'x'),
        test "expect x by offset 27 will not change because of offset is greater 26" 
        (\_ -> decode 27 'x'
            |> Expect.equal 'x'),
        test "expect x by offset 0 will not change" 
        (\_ -> decode 0 'x'
            |> Expect.equal 'x')
    ]