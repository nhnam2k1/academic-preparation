module CaesarP2 exposing (..)

import Caesar exposing(..)
import Char exposing (isAlpha)

normalize : String -> String
normalize str = 
    String.toList str
    |> List.filter isAlpha 
    |> String.fromList

encrypt : Int -> String -> String 
encrypt offset str = 
    String.toList str
    |> List.map (encode offset)  
    |> String.fromList

decrypt : Int -> String -> String
decrypt offset str = 
    String.toList str
    |> List.map (decode offset)  
    |> String.fromList