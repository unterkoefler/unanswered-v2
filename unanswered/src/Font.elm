module Font exposing (decreaseFontSize, fontSize, increaseFontSize)

import Array
import Element exposing (Attribute)
import Element.Font


increaseFontSize : Int -> Int
increaseFontSize currentSize =
    clampSize (currentSize + 1)


decreaseFontSize : Int -> Int
decreaseFontSize currentSize =
    clampSize (currentSize - 1)


clampSize : Int -> Int
clampSize =
    clamp 0 (Array.length fontSizes - 1)



-- indexes in fontSizes to pixel units


translate : Int -> Int
translate n =
    Array.get n fontSizes
        |> Maybe.withDefault 16


fontSizes =
    Array.fromList
        [ 8
        , 10
        , 12
        , 14
        , 16
        , 18
        , 24
        , 36
        , 48
        , 64
        ]


fontSize : Int -> Attribute msg
fontSize n =
    n
        |> clampSize
        |> translate
        |> Element.Font.size
