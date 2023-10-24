module collatz exposing (..)

aboveN : Int -> Int -> Bool
aboveN n x = x > n

double : Int -> Int
double x = x * 2

log3 : Int -> Int -> Int
log3 base target =
    let
        condition x =
            target <= (base ^ x)    
    in
    repeatUntil condition (\x -> x + 1) 0

collatz : Int -> List Int
collatz initial =
    let
        step x =
            if x  modBy 2 == 0 then
                x // 2
            else
                3 * x + 1
    in
    repeatUntil (\current -> current == 1) (\current -> step current :: current) [initial]

repeatUntil : (a -> Bool) -> (a -> a) -> List a -> List a
repeatUntil stop convert ints =
    case ints of
        [] -> []
        x :: items -> 
            let
                result = if stop x then ints 
                    else
                    let
                        newNumber = convert x
                    in 
                    newNumber :: ints
            in
            result
            

