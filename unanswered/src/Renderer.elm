module Renderer exposing (renderPost)

import Colors
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
import Font exposing (fontSize)
import Markdown.Block as Block exposing (HeadingLevel(..), ListItem(..))
import Markdown.Html
import Markdown.Parser exposing (deadEndToString)
import Markdown.Renderer exposing (..)
import Utils exposing (directions0)
import Html


renderPost : Colors.ColorScheme -> Int -> String -> Length -> List (Element msg)
renderPost colorScheme baseFontSize md imageWidth =
    md
        |> Markdown.Parser.parse
        |> Result.mapError (\deadEnds -> deadEnds |> List.map deadEndToString |> String.join "\n")
        |> Result.andThen (\ast -> render (renderer colorScheme baseFontSize imageWidth) ast)
        |> Result.withDefault [ text "parse failed" ]


renderer : Colors.ColorScheme -> Int -> Length -> Renderer (Element msg)
renderer colorScheme baseFontSize imageWidth =
    { heading = renderHeading baseFontSize
    , paragraph = renderParagraph
    , blockQuote = \els -> row [ width fill ] els
    , html = Markdown.Html.oneOf [ captionRenderer baseFontSize ]
    , text = \t -> text t
    , codeSpan = renderCodeSpan
    , strikethrough = \_ -> Element.none
    , strong = renderStrong
    , emphasis = renderEmphasis
    , hardLineBreak = Html.br [] [] |> html
    , link = renderLink colorScheme
    , image = \args -> renderImage args imageWidth
    , unorderedList = renderUnorderedList
    , orderedList = renderOrderedList
    , codeBlock = renderCodeBlock
    , thematicBreak = hr
    , table = \_ -> Element.none
    , tableHeader = \_ -> Element.none
    , tableBody = \_ -> Element.none
    , tableRow = \_ -> Element.none
    , tableCell = \_ _ -> Element.none
    , tableHeaderCell = \_ _ -> Element.none
    }


captionRenderer : Int -> Markdown.Html.Renderer (List (Element msg) -> Element msg)
captionRenderer baseFontSize =
    Markdown.Html.tag "caption" (\caption children -> renderCaption baseFontSize caption)
        |> Markdown.Html.withAttribute "text"


renderCaption : Int -> String -> Element msg
renderCaption baseFontSize caption =
    paragraph
        [ width fill
        , Font.italic
        , fontSize (baseFontSize - 2)
        , centerX
        , Font.center
        , paddingEach { directions0 | bottom = 5 }
        ]
        [ text caption ]


hr : Element msg
hr =
    el
        [ width fill
        , height (px 2)
        , Border.widthEach { directions0 | top = 1 }
        ]
        Element.none


renderHeading : Int -> { level : Block.HeadingLevel, rawText : String, children : List (Element msg) } -> Element msg
renderHeading baseFontSize { level, rawText, children } =
    let
        ( levelNumber, fs ) =
            case level of
                H1 ->
                    ( 1, baseFontSize + 3 )

                H2 ->
                    ( 2, baseFontSize + 2 )

                H3 ->
                    ( 3, baseFontSize + 1 )

                H4 ->
                    ( 4, baseFontSize )

                H5 ->
                    ( 5, baseFontSize - 1 )

                H6 ->
                    ( 6, baseFontSize - 2 )
    in
    case level of
        H6 ->
            -- HACK to center images
            row [ width fill ] children

        _ ->
            paragraph [ Region.heading levelNumber, fontSize fs, paddingEach { directions0 | top = 12 } ]
                children


renderParagraph : List (Element msg) -> Element msg
renderParagraph =
    paragraph [ spacing 12, paddingEach { directions0 | top = 5 } ]


renderCodeSpan : String -> Element msg
renderCodeSpan s =
    el
        [ Font.family [ Font.monospace ]
        , Background.color <| rgb255 150 150 150
        ]
    <|
        text s


renderStrong : List (Element msg) -> Element msg
renderStrong =
    row
        [ Font.bold ]


renderEmphasis : List (Element msg) -> Element msg
renderEmphasis =
    row
        [ Font.italic ]


renderUnorderedList : List (ListItem (Element msg)) -> Element msg
renderUnorderedList items =
    column
        [ spacing 18 ]
    <|
        List.map (\(ListItem _ els) -> renderListItem "-" els) items


renderOrderedList : Int -> List (List (Element msg)) -> Element msg
renderOrderedList _ items =
    column
        [ spacing 18 ]
    <|
        List.indexedMap
            (\i -> renderListItem (String.fromInt (i + 1) ++ "."))
            items


renderListItem : String -> List (Element msg) -> Element msg
renderListItem marker els =
    row [ spacing 12 ]
        [ el [ alignTop, Font.bold ] <| text marker
        , paragraph [ spacing 12 ] els
        ]


renderLink :
    Colors.ColorScheme
    ->
        { title : Maybe String
        , destination : String
        }
    -> List (Element msg)
    -> Element msg
renderLink colorScheme { title, destination } content =
    let
        label =
            case content of
                [ lbl ] ->
                    lbl

                _ ->
                    row [] content
    in
    link
        [ Font.color <| Colors.link colorScheme ]
        { url = destination
        , label = label
        }


renderImage :
    { alt : String
    , src : String
    , title : Maybe String
    }
    -> Length
    -> Element msg
renderImage { alt, src, title } imageWidth =
    el [ width imageWidth, paddingXY 0 5 ] <|
        image [ width imageWidth, height (fill |> maximum 800), centerX, scrollbarY ]
            { description = alt
            , src = "/" ++ src
            }


renderCodeBlock : { body : String, language : Maybe String } -> Element msg
renderCodeBlock { body } =
    let
        paragraphs =
            body
                |> String.split "\n"
                |> List.map
                    (\t -> paragraph [] [ text t ])
    in
    column
        [ Font.family [ Font.monospace ], spacing 12 ]
        paragraphs
