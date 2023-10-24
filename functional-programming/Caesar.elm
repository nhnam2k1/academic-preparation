module Caesar exposing (..)

shiftChar: Int -> Char -> Char
shiftChar offset character =
    let
        isLowerCase = Char.isLower character
        isUpperCase = Char.isUpper character
        currentCode = Char.toCode character
        baseChar =
            if isLowerCase then
                Char.toCode 'a'
            else if isUpperCase then
                Char.toCode 'A'
            else
                currentCode
        calculateShift = currentCode - baseChar + offset + 26
        calculateShiftBy = modBy 26 calculateShift
    in
    if isLowerCase || isUpperCase then
        Char.fromCode (baseChar + calculateShiftBy)
    else 
        character

encode : Int -> Char -> Char
encode offset char =
    if offset < 0 || offset > 26 then 
        char
    else if Char.isAlpha char then
        shiftChar offset char
    else
        char

decode : Int -> Char -> Char
decode offset char =
    if offset < 0 || offset > 26 then 
        char
    else if Char.isAlpha char then
        shiftChar (-offset) char
    else
        char