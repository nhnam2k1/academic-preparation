module Pythagoras exposing (..)

sqr : Int -> Int
sqr num = num^2

isTriple : Int -> Int -> Int -> Bool
isTriple firstLeg secondLeg hypotenuse = 
    let
        sqrFirstLeg = sqr firstLeg
        sqrSecondLeg = sqr secondLeg
        sqrHypotenuse = sqr hypotenuse
    in
    if firstLeg <= 0 
        || secondLeg <= 0 
        || hypotenuse <= 0 then False
    else if sqrFirstLeg + sqrSecondLeg == sqrHypotenuse then True
    else False

leg1 : Int -> Int -> Int
leg1 x y = x^2 - y^2

leg2 : Int -> Int -> Int 
leg2 x y = 2*x*y

hyp : Int -> Int -> Int
hyp x y = x^2 + y^2

pythTriple : (Int, Int) -> (Int, Int,Int)
pythTriple (x, y) =
    if x <= 0 || y <= 0 || x < y then (0, 0, 0)
    else 
    let
        firstLeg = leg1 x y
        secondLeg = leg2 x y
        hypotenuse = hyp x y
    in
    (firstLeg, secondLeg, hypotenuse)

isTripleTuple : (Int, Int, Int) -> Bool
isTripleTuple (firstLeg, secondLeg, hypotenuse) = 
    isTriple firstLeg secondLeg hypotenuse