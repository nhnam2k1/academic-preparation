module MathFunc2 exposing (..)

import MathFunc exposing (..)

derivative : Function -> Function 
derivative f = 
    case f of 
        Const _ -> Const 0
        X -> Const 1
        Plus l r -> 
            let 
                deriLeft = derivative l
                deriRight = derivative r
            in
            case (deriLeft, deriRight) of 
                -- Put the Const first for easy simplify
                (_, Const _) -> Plus deriRight deriLeft 
                _ -> Plus deriLeft deriRight

        Minus l r -> 
            let 
                deriLeft = derivative l
                deriRight = derivative r
            in
            case (deriLeft, deriRight) of 
                -- Put the Const first for easy simplify
                (_, Const x) -> Plus (Const -x) deriLeft  -- (a-b) = -b + a
                _ -> Minus deriLeft deriRight

        Div l r -> 
            let
                dLeft = derivative l
                dRight = derivative r
                firstP = Mult dLeft r 
                secondP = Mult dRight l
                numerator = Minus firstP secondP
                denominator = Poly r 2
            in
            Div numerator denominator

        Mult l r -> 
            let
                dLeft = derivative l
                dRight = derivative r
                firstP = Mult dLeft r 
                secondP = Mult dRight l
            in
            Plus firstP secondP
        
        Poly func e ->
            let 
                left = Const e
                temp = Poly func (e-1)
                chain = derivative func -- Chain rule apply in polynal
                right =  case func of
                            X -> temp
                            _ -> Mult chain temp 
            in
            Mult left right -- It is num * (num of func) * func

-- Product rule special case
-- Line (ax)' = a'x + ax' = 0 + a = a
-- Multiplication by constant (cf)' = c'f + cf' = cf'

-- Quotient Rule special case
-- (5*x)^2 / 5x = 

simplify: Function -> Function
simplify f = 
    case f of
        Plus l r ->
            let
                simpLeft = simplify l
                simpRight = simplify r
            in
            case (simpLeft, simpRight) of
                (Const x, Const y) -> Const (x + y)
                (Const 0, _) -> simpRight -- (0 +)
                (_ , Const 0) -> simpLeft -- (+ 0)

                (Plus (Const x) pairFirst, Const y) -> -- (5 + x) + 5
                    let
                        constant = Const (x + y)
                        newExp = Plus constant pairFirst
                    in
                    simplify newExp

                (Minus (Const x) pairFirst, Const y) -> -- (5 - x) + 5
                    let
                        constant = Const (x + y)
                        newExp = Minus constant pairFirst
                    in
                    simplify newExp

                (Const x, Minus (Const y) pairFirst) -> -- 5 + (5 - x)
                    let
                        constant = Const (x + y)
                        newExp = Minus constant pairFirst
                    in
                    simplify newExp

                (Const x, Plus (Const y) pairFirst) -> -- 5 + (5 + x)
                    let
                        constant = Const (x + y)
                        newExp = Plus constant pairFirst
                    in
                    simplify newExp

                -- (5 + x) + (5 + z) = (5 + 5) + (x + z) New pair
                (Plus (Const x) pairFirst, Plus (Const y) pairSecond) -> 
                    let
                        constant = Const (x + y)
                        rightP = Plus pairFirst pairSecond
                        newExp = Plus constant rightP
                    in
                    simplify newExp

                -- (5 - x) + (5 + z) = 5 - x + 5 + z = (5 + 5) + (z - x) // new pair
                (Minus (Const x) pairFirst, Plus (Const y) pairSecond) -> 
                    let 
                        constant = Const (x + y)
                        rightP = Minus pairSecond pairFirst
                        newExp = Plus constant rightP
                    in
                    simplify newExp

                -- (5 + x) + (5 - z) = 5 + x + 5 - z = (5 + 5) + (x - z) // new pair
                (Plus (Const x) pairFirst, Minus (Const y) pairSecond) -> 
                    let 
                        constant = Const (x + y)
                        rightP = Minus pairFirst pairSecond
                        newExp = Plus constant rightP
                    in
                    simplify newExp

                -- (5 - x) + (5 - z) = 5 - x + 5 - z = (5 + 5) - (x + z) // new pair
                (Minus (Const x) pairFirst, Minus (Const y) pairSecond) -> 
                    let 
                        constant = Const (x + y)
                        rightP = Plus pairFirst pairSecond
                        newExp = Minus constant rightP
                    in
                    simplify newExp

                _ -> Plus simpLeft simpRight

        Minus l r ->
            let
                simpLeft = simplify l
                simpRight = simplify r
            in
            case (simpLeft, simpRight) of
                (Const x, Const y) -> Const (x - y)
                (_ , Const 0) -> simpLeft -- (- 0)

                -- 5 - (5 - x) = (5 - 5) + x 
                (Const x, Minus (Const y) firstP) ->
                    let
                        constant = Const (x - y) 
                        newExp = Plus constant firstP
                    in
                    simplify newExp

                -- 5 - (5 + x) = (5 - 5) - x
                (Const x, Plus (Const y) firstP) -> 
                    let
                        constant = Const (x - y)
                        newExp = Minus constant firstP 
                    in
                    simplify newExp

                -- (5 + x) - 5 = (5 - 5) + x
                (Plus (Const x) firstP, Const y) ->
                    let
                        constant = Const (x - y) 
                        newExp = Plus constant firstP
                    in
                    simplify newExp

                -- (5 - x) - 5 = (5 - 5) - x
                (Minus (Const x) firstP, Const y) ->
                    let
                        constant = Const (x - y) 
                        newExp = Minus constant firstP
                    in
                    simplify newExp

                -- (5 + x) - (5 + z) = 5 + x - 5 - z = (5 - 5) + (x - z)
                (Plus (Const x) firstP, Plus (Const y) secondP) ->
                    let
                        constant = Const (x - y)
                        pair = Minus secondP firstP 
                        newExp = Plus constant pair
                    in
                    simplify newExp

                -- (5 + x) - (5 - z) = 5 + x - 5 + z = (5 - 5) + (x + z)
                (Plus (Const x) firstP, Minus (Const y) secondP) ->
                    let
                        constant = Const (x - y)
                        pair = Plus secondP firstP 
                        newExp = Plus constant pair
                    in
                    simplify newExp

                -- (5 - x) - (5 + z) = 5 - x - 5 - z = (5 - 5) - (x + z)
                (Minus (Const x) firstP, Plus (Const y) secondP) ->
                    let
                        constant = Const (x - y)
                        pair = Plus secondP firstP 
                        newExp = Minus constant pair
                    in
                    simplify newExp

                -- (5 - x) - (5 - z) = 5 - x - 5 + z = (5 - 5) + (z - x)
                (Minus (Const x) firstP, Minus (Const y) secondP) ->
                    let
                        constant = Const (x - y)
                        pair = Minus secondP firstP
                        newExp = Plus constant pair
                    in
                    simplify newExp

                _ -> Minus simpLeft simpRight

        Div l r -> 
            let 
                simpLeft = simplify l
                simpRight = simplify r
            in
            case (simpLeft, simpRight) of
                (_, Const 1) -> simpLeft -- (/1)
                _ -> Div simpLeft simpRight
        
        Mult l r -> 
            let 
                simpLeft = simplify l
                simpRight = simplify r
            in
            case (simpLeft, simpRight) of
                (Const x, Const y) -> Const (x * y)
                (_, Const 1) -> simpLeft -- (*1)
                (Const 1, _) -> simpRight -- (1*)
                (_, Const 0) -> Const 0 -- (*0)
                (Const 0, _) -> Const 0 -- (0*)

                -- 5 * (5 * x) = (5 * 5) * x
                (Const x, Mult (Const y) firstP) ->
                    let
                        constant = Const (x * y) 
                        newExp = Mult constant firstP
                    in
                    simplify newExp

                -- (5 * x) * 5 = (5 * 5) * x
                (Mult (Const x) firstP, Const y) ->
                    let
                        constant = Const (x * y) 
                        newExp = Mult constant firstP
                    in
                    simplify newExp

                -- (5 * x) * (5 * z) = (5 * 5) * (x * z)
                (Mult (Const x) firstP, Mult (Const y) secondP) ->
                    let
                        constant = Const (x * y) 
                        pair = Mult firstP secondP
                        newExp = Mult constant pair
                    in
                    simplify newExp
                
                _ -> Mult simpLeft simpRight
            
        Poly func e -> 
            if e == 0 then Const 1 else -- (^0)
            let
                simple = simplify func 
            in
            if e == 1 then simple -- (^1)
            else case simple of 
                    Const x -> Const (x^e)
                    _ -> Poly simple e
        
        _ -> f -- Const or X