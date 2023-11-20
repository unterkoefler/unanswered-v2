module Utils exposing (borderBetween, borderBetweenRow, directions0)

import Element exposing (..)
import Element.Border as Border
import Url exposing (Url)


directions0 =
    { left = 0, right = 0, top = 0, bottom = 0 }


borderBetween : List (Element msg) -> List (Element msg)
borderBetween elements =
    case elements of
        [] ->
            []

        [ element ] ->
            [ element ]

        element :: rest ->
            el [ Border.widthEach { directions0 | bottom = 1 }, width fill ]
                element
                :: borderBetween rest


borderBetweenRow : List (Element msg) -> List (Element msg)
borderBetweenRow elements =
    case elements of
        [] ->
            []

        [ element ] ->
            [ element ]

        element :: rest ->
            el [ Border.widthEach { directions0 | right = 1 }, paddingEach { directions0 | right = 8 } ]
                element
                :: borderBetweenRow rest
