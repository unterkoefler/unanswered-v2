module Shared exposing (Data, Model, Msg(..), SharedMsg(..), template, montserrat, sourceSerifPro, colorSchemeSwitcher)

import Array
import BackendTask exposing (BackendTask)
import Browser.Events exposing (onResize)
import Browser.Navigation
import Effect exposing (Effect)
import FatalError exposing (FatalError)
import Html exposing (Html)
import Html.Events
import Pages.Flags
import Pages.PageUrl exposing (PageUrl)
import UrlPath exposing (UrlPath)
import Route exposing (Route)
import Random
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
import Posts
import Url.Builder
import Utils exposing (..)
import FeatherIcons
import Font exposing (increaseFontSize, decreaseFontSize, fontSize)


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
    | ChangeColorScheme Colors.ColorScheme
    | CloseMenu
    | IncreaseFontSize
    | DecreaseFontSize
    | GoToRandomPost String
    | GenerateRandomPost (List String)


type alias Data =
    { slugs : List String }


type SharedMsg
    = NoOp


type alias Model =
    { showMenu : Bool
    , colorScheme : Colors.ColorScheme
    , baseFontSize : Int
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
                    Flags Colors.Light

                Pages.Flags.BrowserFlags value ->
                    decodeFlags value
    in
    ( 
        { showMenu = False
        , colorScheme = parsedFlags.colorScheme
        , baseFontSize = 4
        }
    , Effect.none
    )

type alias Flags =
    { colorScheme : Colors.ColorScheme
    }

decodeFlags : Decode.Value -> Flags
decodeFlags value =
    case Decode.decodeValue flagDecoder value of
        Err a ->
            { colorScheme = Colors.Light }

        Ok flags ->
            flags

flagDecoder : Decoder Flags
flagDecoder =
    Decode.map
        Flags
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

        ChangeColorScheme newScheme ->
            ( { model | colorScheme = newScheme }, Effect.none )

        IncreaseFontSize ->
            ( { model | baseFontSize = increaseFontSize model.baseFontSize }
            , Effect.none
            )

        DecreaseFontSize ->
            ( { model | baseFontSize = decreaseFontSize model.baseFontSize }
            , Effect.none
            )

        GenerateRandomPost slugs ->
            ( model
            , Effect.Cmd
                (Random.generate GoToRandomPost
                    (randomSlugGenerator slugs)
                )
            )

        GoToRandomPost slug ->
            ( model
            , Effect.Cmd
                (Browser.Navigation.load
                    (Url.Builder.absolute
                        [ slug ]
                        []
                    )
                )
            )

randomSlugGenerator : List String -> Random.Generator String 
randomSlugGenerator slugs =
    let
        values = Array.fromList slugs
    in
    Random.int 0 ((Array.length values) - 1)
        |> Random.map (\i -> Array.get i values)
        |> Random.map (Maybe.withDefault "the-hearse")

subscriptions : UrlPath -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none


data : BackendTask FatalError Data
data =
    Posts.posts
        |> BackendTask.map (List.map .slug)
        |> BackendTask.map (\slugs -> { slugs = slugs })


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
            (frame pageView model toMsg sharedData)
        ]
    , title = pageView.title
    }

frame : View msg -> Model -> (Msg -> msg) -> Data -> Element msg
frame pageView model toMsg sharedData =
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
                pageView.previous
                pageView.next
                toMsg
                sharedData

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
            , el
                [ width fill
                , centerX
                , paddingXY 24 0
                ]
                child
            ]

footer : Model -> Maybe String -> Maybe String -> Data -> Element Msg
footer model previous next sharedData =
    wrappedRow
        [ Border.color <| Colors.accent model.colorScheme
        , Border.widthEach { top = 1, bottom = 0, left = 0, right = 0 }
        , paddingXY 36 18
        , Font.size 14
        , spaceEvenly
        , Font.color <| Colors.neutralOnSecondary model.colorScheme
        , width fill
        ]
        [ el [ paddingEach { directions0 | right = 8 } ] <| arrowLeft model previous
        , el [ paddingXY 8 0 ] <| randomPostButton sharedData.slugs
        , subscribeLink [ paddingXY 8 0 ]
        , link [ paddingXY 8 0 ] { url = Url.Builder.absolute [] [], label = text "Home" }
        , el [ paddingEach { directions0 | left = 8 } ] <| arrowRight model next
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
            , menuOption Route.Search "Search"
            , colorSchemeSwitcher colorScheme
                [ paddingXY 0 24 ]
            , subscribeLink [ paddingXY 0 24 ]
            ]

menuOption : Route -> String -> Element Msg
menuOption route lbl =
    link
        [ paddingXY 0 24
        , width fill
        ]
        { url = route |> Route.toPath |> UrlPath.toRelative
        , label = text lbl
        }

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

randomPostButton : List String -> Element Msg
randomPostButton slugs = 
    Input.button
        []
        { label = text "Random"
        , onPress = Just (GenerateRandomPost slugs)
        }

subscribeLink : List (Attribute msg) -> Element msg
subscribeLink attrs =
    newTabLink
        attrs
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

navButtons : Model -> Maybe String -> Maybe String -> Element msg
navButtons model previous next =
    row
        [ alignRight
        , paddingEach { directions0 | right = 0 }
        , spacing 12
        ]
        [ arrowLeft model previous
        , arrowRight model next
        ]

arrowRight : Model -> Maybe String -> Element msg
arrowRight =
    arrow FeatherIcons.arrowRight "next" identity

arrowLeft : Model -> Maybe String -> Element msg
arrowLeft =
    arrow FeatherIcons.arrowLeft "previous" List.reverse

arrow : FeatherIcons.Icon -> String -> (List (Element msg) -> List (Element msg)) -> Model -> Maybe String -> Element msg
arrow ic label reverser model slug =
    case slug of
        Nothing ->
            row [ spacing 8 ]
                (reverser
                    [ text <| "there is no " ++ label
                    , icon [ Font.color <| Colors.disabled model.colorScheme ] ic
                    ]
                )

        Just s ->
            link []
                { url = "/" ++ s
                , label = 
                    row [ spacing 8 ]
                        (reverser
                            [ text label
                            , icon [] ic
                            ]
                        )
                }

icon attrs i =
    el attrs (i |> FeatherIcons.toHtml [] |> html)

articleFrame : Element msg -> Model -> Maybe String -> Maybe String -> (Msg -> msg) -> Data -> Element msg
articleFrame post model previous next toMsg sharedData =
    column
        [ width fill
        , height fill
        ]
        [ column [ width fill ]
                [ Element.map toMsg <| header model
                ]
        , row [ width fill, height fill ]
            [ el
                [ width (fillPortion 95), alignTop ]
                (content
                    (column
                        [ width fill ]
                        [ Element.map toMsg (textControls model)
                        , post
                        ]
                    )
                )
            , sideBar model.colorScheme
            ]
        , el [ width fill, alignBottom ] 
            (Element.map toMsg (footer model previous next sharedData))
        ]

textControls : Model -> Element Msg
textControls model =
    let 
        { colorScheme, baseFontSize } = model
    in
    el
        [ paddingXY 12 0
        , centerX
        , width (fill |> maximum 800)
        ]
        <|
        wrappedRow
            [ spacing 12
            , width fill
            , centerX
            , paddingXY 0 24
            , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
            , Border.dotted
            , Border.color (Colors.primary colorScheme)
            ]
            (borderBetweenRow
                [ colorSchemeSwitcher colorScheme [ fontSize (baseFontSize - 2) ]
                , fontSizeChanger baseFontSize IncreaseFontSize "Increase"
                , fontSizeChanger baseFontSize DecreaseFontSize "Decrease"
                ]
            )

fontSizeChanger : Int -> msg -> String -> Element msg
fontSizeChanger baseFontSize msg verb =
    let
        lbl = 
            verb ++ " font size"
    in
    Input.button
        [ fontSize (baseFontSize - 2) ]
        { label = text lbl
        , onPress = Just msg
        }


content : Element msg -> Element msg
content =
    el
        [ centerX
        , width fill
        , paddingXY 24 0
        ]

sideBar : Colors.ColorScheme -> Element msg
sideBar colorScheme =
    el
        [ width (fillPortion 5)
        , Background.gradient 
            { angle = pi
            , steps = [ Colors.accent colorScheme, Colors.secondary colorScheme ] 
            }
        , alignRight
        , height fill
        ]
        <|
            Element.none
