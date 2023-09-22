module Shared exposing (Data, Model, Msg(..), SharedMsg(..), template, montserrat, sourceSerifPro)

import BackendTask exposing (BackendTask)
import Browser.Events exposing (onResize)
import Effect exposing (Effect)
import FatalError exposing (FatalError)
import Html exposing (Html)
import Html.Events
import Pages.Flags
import Pages.PageUrl exposing (PageUrl)
import UrlPath exposing (UrlPath)
import Route exposing (Route)
import SharedTemplate exposing (SharedTemplate)
import View exposing (View)
import Element exposing (..)
import Element.Background as Background
import Element.Region as Region
import Element.Input as Input
import Element.Border as Border
import Element.Font as Font
import Colors
import Json.Decode as Decode exposing (Decoder)
import Utils exposing (..)


template : SharedTemplate Msg Model Data msg
template =
    { init = init
    , update = update
    , view = view
    , data = data
    , subscriptions = subscriptions
    , onPageChange = Just (\_ -> CloseMenu)
    }


type Msg
    = SharedMsg SharedMsg
    | MenuToggled
    | WindowResized Int
    | ChangeColorScheme Colors.ColorScheme
    | CloseMenu


type alias Data =
    ()


type SharedMsg
    = NoOp


type alias Model =
    { showMenu : Bool
    , colorScheme : Colors.ColorScheme
    , width : Int
    }


init :
    Pages.Flags.Flags
    ->
        Maybe
            { path :
                { path : UrlPath
                , query : Maybe String
                , fragment : Maybe String
                }
            , metadata : route
            , pageUrl : Maybe PageUrl
            }
    -> ( Model, Effect Msg )
init flags maybePagePath =
    let
        parsedFlags =
            case flags of
                Pages.Flags.PreRenderFlags ->
                    Flags 1200 Colors.Light -- TODO: do something better here

                Pages.Flags.BrowserFlags value ->
                    decodeFlags value
    in
    ( { showMenu = False, colorScheme = parsedFlags.colorScheme, width = parsedFlags.width }
    , Effect.none
    )

type alias Flags =
    { width : Int
    , colorScheme : Colors.ColorScheme
    }

decodeFlags : Decode.Value -> Flags
decodeFlags value =
    case Decode.decodeValue flagDecoder value of
        Err a ->
            { width = 1200, colorScheme = Colors.Light }

        Ok flags ->
            flags

flagDecoder : Decoder Flags
flagDecoder =
    Decode.map2
        Flags
        (Decode.field "width" Decode.int)
        (Decode.field "colorScheme" decodeColorScheme)

decodeColorScheme : Decoder Colors.ColorScheme
decodeColorScheme =
    Decode.map
        (\str -> 
            case str of
                "Light" ->
                    Colors.Light

                "Dark" ->
                    Colors.Dark

                _ ->
                    Colors.Light
        )
        Decode.string


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        SharedMsg globalMsg ->
            ( model, Effect.none )

        MenuToggled ->
            ( { model | showMenu = not model.showMenu }, Effect.none )

        CloseMenu ->
            ( { model | showMenu = False }, Effect.none )

        WindowResized w ->
            ( { model | width = w }, Effect.none )

        ChangeColorScheme newScheme ->
            ( { model | colorScheme = newScheme }, Effect.none )


subscriptions : UrlPath -> Model -> Sub Msg
subscriptions _ _ =
    onResize (\w h -> WindowResized w)


data : BackendTask FatalError Data
data =
    BackendTask.succeed ()


view :
    Data
    ->
        { path : UrlPath
        , route : Maybe Route
        }
    -> Model
    -> (Msg -> msg)
    -> View msg
    -> { body : List (Html msg), title : String }
view sharedData page model toMsg pageView =
    { body =
        [ Element.layout 
            [ montserrat
            , Font.color (Colors.primary model.colorScheme)
            , Background.color (Colors.secondary model.colorScheme)
            ] 
            (frame pageView model toMsg)
        ]
    , title = pageView.title
    }

frame : View msg -> Model -> (Msg -> msg) -> Element msg
frame pageView model toMsg =
    case pageView.pageLayout of
        View.HomePage ->
            homeFrame
                pageView.body
                model
                toMsg

        View.PostPage ->
            articleFrame
                pageView.body
                model

homeFrame : Element msg -> Model -> (Msg -> msg) -> Element msg
homeFrame child model toMsg =
        column
            [ width fill
            , spacing 24
            , paddingEach { left = 0, right = 0, top = 0, bottom = 36 }
            ]
            [ column [ width fill ]
                [ Element.map toMsg <| header model
                , subheader model.colorScheme
                ]
            , child
            ]

header : Model -> Element Msg
header model =
    row
        [ Background.color <| Colors.accent model.colorScheme
        , padding 24
        , width fill
        , Font.color Colors.white
        ]
        [ heading
        , menu model.colorScheme model.showMenu
        ]

heading : Element msg
heading = 
    link
        [ Region.heading 1
        , Font.size 36
        , sourceSerifPro
        , paddingEach { left = 0, right = 12, top = 0, bottom = 0 }
        ]
        { label = text "Unanswered"
        , url = "/"
        }

subheader : Colors.ColorScheme -> Element msg
subheader colorScheme =
    paragraph
        [ Region.heading 2
        , Font.size 18
        , width fill
        , paddingXY 24 12
        , Background.color <| Colors.neutral colorScheme
        , Font.color <| Colors.white
        ]
        [ text "Where I type and scream my thoughts into the void, unanswered" ]

menu : Colors.ColorScheme -> Bool -> Element Msg
menu colorScheme showMenu =
    let
        modal = 
            if showMenu then
                menuModal colorScheme

            else
                Element.none
    in
    el 
        [ alignRight
        , onLeft modal
        ]
    <|
        Input.button [ Font.size 30 ]
            { label = text "⋮" 
            , onPress = Just MenuToggled
            }

menuModal : Colors.ColorScheme -> Element Msg
menuModal colorScheme =
    el
        [ width shrink
        , height shrink
        , Background.color <| Colors.accentDark colorScheme
        , Border.color <| Colors.accent colorScheme
        , Border.width 1
        , Border.rounded 6
        , paddingXY 45 5
        ]
        (menuOptions colorScheme)

menuOptions : Colors.ColorScheme -> Element Msg
menuOptions colorScheme =
    column
        [ Font.size 24
        , sourceSerifPro
        ]
    <|
        borderBetween
            [ menuOption Route.WhatIsThis "What is this?"
            , menuOption Route.WhoAmI "Who am I?"
            , menuOption Route.ContactMe "Contact me"
            , menuOption Route.Categories "Posts by category"
            , showSearchToggler
            , colorSchemeSwitcher colorScheme
                [ paddingXY 0 24 ]
            , subscribeLink
            ]

menuOption : Route -> String -> Element Msg
menuOption route lbl =
    link
        [ paddingXY 0 24
        ]
        { url = route |> Route.toPath |> UrlPath.toRelative
        , label = text lbl
        }

showSearchToggler : Element msg
showSearchToggler =
    el [ paddingXY 0 24 ] (text "Search" ) -- TODO: move to its own page

colorSchemeSwitcher : Colors.ColorScheme -> List (Attribute Msg) -> Element Msg
colorSchemeSwitcher colorScheme attrs =
    let
        ( lbl, nextScheme) =
            case colorScheme of
                Colors.Light ->
                    ( "Switch to dark mode 🌚", Colors.Dark )

                Colors.Dark ->
                    ( "Switch to light mode 🌞", Colors.Light )
    in
    Input.button
        attrs
        { label = text lbl
        , onPress = Just <| ChangeColorScheme nextScheme
        }

subscribeLink : Element msg
subscribeLink =
    newTabLink
        [ paddingXY 0 24 ]
        { url = "http://eepurl.com/g_Wf8D"
        , label = text "Subscribe"
        }

sourceSerifPro =
    Font.family
        [ Font.typeface "SourceSerifPro"
        , Font.serif
        ]

montserrat =
    Font.family
        [ Font.typeface "Montserrat"
        , Font.sansSerif
        ]

articleFrame : Element msg -> Model -> Element msg
articleFrame post model =
    column
        [ width fill
        , height fill
        ]
        [ row [ width fill, height fill ]
            [ el [ width fill, alignTop ] <|
                content model.width 70 post
            , sideBar model.colorScheme model.width
            ]
        , el [ width fill, alignBottom ] <| text "TODO: footer"
        ]


content : Int -> Int -> Element msg -> Element msg
content w percent =
    el
        [ centerX
        , width (pct w percent)
        ]

sideBar : Colors.ColorScheme -> Int -> Element msg
sideBar colorScheme w =
    el
        [ width (maximum 96 (pct w 5))
        , Background.color <| Colors.accent colorScheme
        , alignRight
        , height fill
        ]
        <|
                text ""
