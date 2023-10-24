module PythagorasP2 exposing (..)

import Pythagoras exposing(..)

pythTriplesMap : List (Int, Int) -> List (Int, Int, Int) 
pythTriplesMap tuples =
    List.map pythTriple tuples 


pythTriplesRec : List (Int, Int) -> List (Int, Int, Int)
pythTriplesRec tuples =
    case tuples of
        [] -> []
        item :: others -> List.append 
                            [(pythTriple item)] 
                            (pythTriplesRec others)


arePythTriplesFilter : List (Int, Int, Int) -> List (Int, Int, Int) 
arePythTriplesFilter pythTriples = 
    List.filter isTripleTuple pythTriples

arePythTriplesRec : List (Int, Int, Int) -> List (Int, Int, Int)
arePythTriplesRec pythTriples = 
    case pythTriples of
        [] -> []
        item :: others -> 
            let 
                previousTuple = arePythTriplesRec others
                filteredTuple = if isTripleTuple item 
                                then List.append [item] previousTuple
                                else previousTuple
                
            in
            filteredTuple
            