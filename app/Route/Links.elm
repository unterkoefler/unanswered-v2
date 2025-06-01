module Route.Links exposing (ActionData, Data, Model, Msg, route)

import BackendTask exposing (BackendTask)
import FatalError exposing (FatalError)
import Font exposing (fontSize)
import Html.Attributes
import Head
import Head.Seo as Seo
import Pages.Url
import PagesMsg exposing (PagesMsg)
import UrlPath
import Route
import RouteBuilder exposing (App, StatefulRoute)
import Shared
import View exposing (View)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
import Element.Input as Input
import Effect exposing (Effect)
import Colors
import Posts
import Date
import Utils exposing (..)
import Post
import NonBlogPost exposing (NonBlogPost)

type alias Model =
    { clickCount : Int }

type Msg
    = ClickLink

type alias RouteParams =
    {}

type alias Data =
    { post: NonBlogPost
    }

type alias ActionData =
    {}

route : StatefulRoute RouteParams Data ActionData Model Msg
route = 
    RouteBuilder.single
        { head = head
        , data = data
        }
        |> RouteBuilder.buildWithLocalState 
            { view = view
            , init = init
            , update = update
            , subscriptions = (\_ _ _ _ -> Sub.none )
            }

data : BackendTask FatalError Data
data =
    BackendTask.map Data
        (NonBlogPost.load "content/posts/2025-05-31-links.md")

head : 
    App Data ActionData RouteParams
    -> List Head.Tag
head app =
    seoSummary
        { title = "Links"
        , description = "Links I like"
        , imageOverride = Nothing
        }
        |> Seo.website

view :
    App Data ActionData RouteParams
    -> Shared.Model
    -> Model
    -> View (PagesMsg Msg)
view app shared model =
    { title = "Unanswered.blog - Links"
    , pageLayout = View.PostPage
    , body = content shared app.data.post model
    , next = Nothing
    , previous = Nothing
    }

content : Shared.Model -> NonBlogPost -> Model -> Element (PagesMsg Msg)
content shared post model =
    column
        [ centerX
        , spacingXY 0 24
        , width (fill |> maximum 800)
        , paddingXY 0 48
        , alignTop
        ]
        [ Post.viewTitle post.title
        , paragraph 
            [ spacingXY 0 18
            , fontSize shared.baseFontSize
            , width (fill |> maximum 800)
            ]
            [ text "One of the cool parts about the Internet is that there are "
            , sneakyLink shared.colorScheme shared.baseFontSize model
            , text ". If you can catch 'em, you can click on 'em and they'll take you places."
            ]
        , Post.viewContent shared.colorScheme shared.baseFontSize (fill |> maximum 800) post.body
        ]

sneakyLink : Colors.ColorScheme -> Int -> Model -> Element (PagesMsg Msg)
sneakyLink colorScheme baseFontSize model =
    let
        styles = 
            [ Font.color <| Colors.link colorScheme
            , Font.underline
            ]
        floatingButton moves = Input.button
                (styles ++ moves ++
                    [ fontSize (baseFontSize + model.clickCount)
                    , htmlAttribute <| Html.Attributes.style "z-index" "100"
                    ]
                )
                { onPress = Just (PagesMsg.fromMsg ClickLink)
                , label = text "links"
                } 

        currentMoves = case model.clickCount of
            1 ->
                [ moveDown 48.0 ]

            2 -> 
                [ moveDown 48.0, moveLeft 90.0 ]

            3 ->
                [ moveLeft 60.0, moveUp 90.0 ]

            4 -> 
                [ moveDown 100.0 ]

            5 -> 
                [ moveRight 20.0 ]
            
            6 ->
                [ moveLeft 60.0, moveDown 400.0 ]

            _ ->
                []
    in
    case model.clickCount of
        7 ->
            link
                styles
                { url = "https://en.wikipedia.org/wiki/URL"
                , label = text "links"
                }
        0 ->
            floatingButton []
        _ ->
            el
                [ inFront <| floatingButton currentMoves
                , Font.underline
                ]
                (text "links")

init :
    App Data ActionData RouteParams
    -> Shared.Model
    -> ( Model, Effect Msg )
init app shared =
    ( { clickCount = 0 }, Effect.none )

update :
    App Data ActionData RouteParams
    -> Shared.Model
    -> Msg
    -> Model
    -> ( Model, Effect Msg )
update app shared msg model =
    ( { model | clickCount = model.clickCount + 1 }, Effect.none )

