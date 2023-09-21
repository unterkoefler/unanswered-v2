module Route.Index exposing (ActionData, Data, Model, Msg, route)

import BackendTask exposing (BackendTask)
import FatalError exposing (FatalError)
import Head
import Head.Seo as Seo
import Pages.Url
import PagesMsg exposing (PagesMsg)
import UrlPath
import Route
import RouteBuilder exposing (App, StatelessRoute)
import Shared
import View exposing (View)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
import Element.Input as Input
import Colors
import Posts


type alias Model =
    {}


type alias Msg =
    ()


type alias RouteParams =
    {}


type alias Data =
    { posts : List Posts.Post
    }


type alias ActionData =
    {}


route : StatelessRoute RouteParams Data ActionData
route =
    RouteBuilder.single
        { head = head
        , data = data
        }
        |> RouteBuilder.buildNoState { view = view }


data : BackendTask FatalError Data
data =
    BackendTask.succeed Data
        |> BackendTask.andMap
            (Posts.posts)


head :
    App Data ActionData RouteParams
    -> List Head.Tag
head app =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = "elm-pages"
        , image =
            { url = [ "images", "icon-png.png" ] |> UrlPath.join |> Pages.Url.fromPath
            , alt = "elm-pages logo"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = "Welcome to elm-pages!"
        , locale = Nothing
        , title = "elm-pages is running"
        }
        |> Seo.website


view :
    App Data ActionData RouteParams
    -> Shared.Model
    -> View (PagesMsg Msg)
view app shared =
    { title = "elm-pages is running"
    , body =
        column
            [ width fill
            , spacing 24
            , paddingEach { left = 0, right = 0, top = 0, bottom = 36 }
            ]
            [ column [ width fill ]
                [ header shared
                , subheader shared.colorScheme
                ]
            , postPreviews app.data.posts
            ]

        --[ Html.h1 [] [ Html.text "elm-pages is up and running!" ]
        --, Html.p []
        --    [ Html.text <| "The message is: " ++ app.data.message
        --    ]
        --, Route.Blog__Slug_ { slug = "hello" }
        --    |> Route.link [] [ Html.text "My blog post" ]
        --]
    }


header : Shared.Model -> Element msg
header shared =
    row
        [ Background.color <| Colors.accent shared.colorScheme
        , padding 24
        , width fill
        , Font.color Colors.white
        ]
        [ heading
        , menu shared.colorScheme shared.showMenu
        ]

heading = 
    link
        [ Region.heading 1
        , Font.size 36
        , Shared.sourceSerifPro
        , paddingEach { left = 0, right = 12, top = 0, bottom = 0 }
        ]
        { label = text "Unanswered"
        , url = "/" -- TODO: use helper
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

menu : Colors.ColorScheme -> Bool -> Element msg
menu colorScheme showMenu =
    let
        modal = Element.none -- TODO: build modal
    in
    el 
        [ alignRight
        , onLeft modal
        ]
    <|
        Input.button [ Font.size 30 ]
            { label = text "⋮" 
            , onPress = Nothing
            }

widthTemp = 1377 -- TODO make dynamic

postPreviews : List Posts.Post -> Element msg
postPreviews posts =
    column
        [ Border.widthEach { top = 0, bottom = 0, left = 1, right = 1 }
        , spacing 36
        , width (fill |> maximum 800)
        , paddingXY  (widthTemp * 6 // 100) 20
        , centerX
        ]
    <|
        case posts of
            [] ->
                [ paragraph [ Font.italic ] [ text "No posts found" ] ]

            _ ->
                List.map Posts.preview posts -- TODO: pagination
