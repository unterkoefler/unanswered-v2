module Post exposing (view)

import Element exposing (..)
import Element.Font as Font
import Element.Region as Region
import Element.Input as Input
import Element.Background as Background
import Element.Border as Border
import Font exposing (fontSize)
import Shared
import Date exposing (Date)
import Utils exposing (..)
import Colors
import Renderer exposing (renderPost)

type alias Post =
    { title : String
    , date : Maybe Date
    , body : String
    }


view : Shared.Model -> Post -> Element msg
view sharedModel =
    viewHelper
        sharedModel.colorScheme
        3 -- TODO: font size controls
        Element.none -- TODO: control bar in general
        (pct sharedModel.width 70 |> maximum 800)


viewHelper : Colors.ColorScheme -> Int -> Element msg -> Length -> Post -> Element msg
viewHelper colorScheme baseFontSize controls w { title, date, body } =
    column [ centerX, spacingXY 0 24, width w, paddingXY 0 48, alignTop ]
        [ viewTitle title
        , viewDate date
        , controls
        , viewContent colorScheme baseFontSize w body
        ]


viewTitle : String -> Element msg
viewTitle title =
    paragraph
        [ Region.heading 1
        , Font.size 36
        , Font.underline
        , paddingXY 0 24
        ]
        [ text title ]


viewDate : Maybe Date -> Element msg
viewDate date =
    case date of
        Nothing ->
            Element.none

        Just d ->
            paragraph
                [ Font.italic
                , Font.size 14
                ]
                [ text <| Date.format "EEEE, MMMM d, yyyy" d ]


viewContent : Colors.ColorScheme -> Int -> Length -> String -> Element msg
viewContent colorScheme baseFontSize w content =
    textColumn
        [ spacingXY 0 18
        , fontSize baseFontSize
        , width w
        ]
    <|
        renderPost colorScheme baseFontSize content w
