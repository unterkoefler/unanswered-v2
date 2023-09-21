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
    , onPageChange = Nothing
    }


type Msg
    = SharedMsg SharedMsg
    | MenuClicked
    | WindowResized Int


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

        MenuClicked ->
            ( { model | showMenu = not model.showMenu }, Effect.none )

        WindowResized w ->
            ( { model | width = w }, Effect.none )


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
            (articleBody
                -- TODO: support the other body types
                pageView.body
                model
            )
        ]
    , title = pageView.title
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

articleBody : Element msg -> Model -> Element msg
articleBody post model =
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
