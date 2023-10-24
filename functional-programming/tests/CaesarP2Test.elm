module CaesarP2Test exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import CaesarP2 exposing (..)

normalizeSuite : Test
normalizeSuite = describe "normalize"
    [
        test "expect 'Hello, Fontys' to 'HelloFontys'" 
        (\_ -> normalize "Hello, Fontys!"
            |> Expect.equal "HelloFontys"),
        test "expect '#!@#$' to empty string" 
        (\_ -> normalize "#!@#$"
            |> Expect.equal ""),
        test "expect 'Fontys' to 'Fontys'" 
        (\_ -> normalize "Fontys"
            |> Expect.equal "Fontys"),
        test "expect ' ' (one space) to empty string" 
        (\_ -> normalize " "
            |> Expect.equal "")
    ]

encryptSuite : Test
encryptSuite = describe "encrypt"
    [
        test "expect 'Hello, Fontys' to 'OlssvMvuafz'" 
        (\_ -> encrypt 7 (normalize "Hello, Fontys!")
            |> Expect.equal "OlssvMvuafz"),
        test "expect '#!@#$' to empty string" 
        (\_ -> encrypt 7 (normalize "")
            |> Expect.equal "")
    ]

decryptSuite : Test
decryptSuite = describe "decrypt"
    [
        test "expect 'OlssvMvuafz' to 'OlssvMvuafz'" 
        (\_ -> decrypt 7 "OlssvMvuafz" 
            |> Expect.equal "HelloFontys"),
        test "expect '' to empty string" 
        (\_ -> decrypt 7 ""
            |> Expect.equal "")
    ]