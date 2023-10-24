module Nam exposing (..)

-- steps:
--      install Elm: https://guide.elm-lang.org/install/elm.html
--      start a terminal
--      run "elm init"
--      copy this file to the new ./src directory
--      rename this file to your own name, and rename the module (see top line above) accordingly


import Html exposing (Html)

-- your functions:

cube: Int -> Int
cube x =
    x^3

reverse: List a -> List a
reverse xs =
    List.reverse xs

reverseCube: List Int -> List Int
reverseCube xs =
    reverse (List.map cube xs)


-- collecting results for printing:

-- arbitrary list:
my_list = [2, 7, 5, 1, 42, 73, 6, 19]

my_results: List String
my_results =
    [
        "-- Hello-Elm output --\n\n  cube calculations:",
        pr <| cube -3,
        pr <| cube 25,
        "  reversing:",
        pr <| my_list ,
        pr <| reverse my_list,
        pr <| reverseCube my_list,
        
        "\n-- end --"
    ] 
    
-- Boiler-plate below:

-- update this values for long output lines, or small browser windows
page_width = 80

to_wrap: String -> String
to_wrap my_value =
    if (String.length my_value <= page_width) then
        (String.left page_width my_value)
    else
        (String.left page_width my_value) ++ ("\n") ++ to_wrap (String.dropLeft page_width my_value)

to_div: String -> Html msg
to_div my_value = 
    Html.div [] [(to_wrap my_value) |> Html.text]

pr = Debug.toString

main: Html msg
main = Html.pre 
        []
        (List.map to_div my_results)
    
