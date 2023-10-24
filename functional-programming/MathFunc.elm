module MathFunc exposing (..)
import Debug exposing (toString)

type Function = Poly Function Int
    | Mult Function Function
    | Div Function Function
    | Plus Function Function
    | Minus Function Function
    | Const Int
    | X

print : Function -> String
print f = 
    case f of 
        X -> "x"
        Const a -> toString(a)
        Poly fun exp ->
            let
                expression = print(fun)
                exponential = toString(exp)
            in
            String.concat ["(", expression, " ^ ", exponential, ")"]
        Mult l r -> 
            let 
                fa = print(l)
                fb = print(r)
            in
            String.concat ["(", fa, " * ", fb  ,")"]
        Div l r ->  
            let 
                fa = print(l)
                fb = print(r)
            in
            String.concat ["(", fa, " / ", fb  ,")"]
        Plus l r -> 
            let 
                fa = print(l)
                fb = print(r)
            in
            String.concat ["(", fa, " + ", fb  ,")"]
        Minus l r -> 
            let 
                fa = print(l)
                fb = print(r)
            in
            String.concat ["(", fa, " - ", fb  ,")"]

eval : Float -> Function -> Float
eval variable func =
    case func of 
        X -> variable
        Const a -> toFloat a
        Poly fun exp ->
            let
                lvalue = eval variable fun
            in
            lvalue ^ toFloat exp
        Mult l r -> 
            let 
                lvalue = eval variable l
                rvalue = eval variable r
            in
            lvalue * rvalue
        Div l r ->  
            let 
                lvalue = eval variable l
                rvalue = eval variable r
            in
            lvalue / rvalue
        Plus l r -> 
            let 
                lvalue = eval variable l
                rvalue = eval variable r
            in
            lvalue + rvalue
        Minus l r -> 
            let 
                lvalue = eval variable l
                rvalue = eval variable r
            in
            lvalue - rvalue

viewY : Float -> Int -> Int -> String
viewY target ymin ymax =
    if ymin > ymax then ""
    else 
        let
            previousP = viewY target ymin (ymax - 1)
            currentP = if (toFloat ymax) >= target then "-" else "*"
        in
        String.concat [previousP, currentP]

graph : Function -> Int -> Int -> Int -> Int -> String
graph f xmin xmax ymin ymax =
    if xmin > xmax then "" 
    else
        let
            evalF = eval (toFloat xmin) f
            line = viewY evalF ymin ymax
            newGraph = graph f (xmin+1) xmax ymin ymax
        in
        String.concat [line, "\n", newGraph]

sample = Plus (Mult (Plus (Const 3) X) (Minus X (Poly X 5))) (Const 2)
fresult = print sample

testF = eval 2 sample

grStr = String.lines (graph sample 0 5 -150 -120)