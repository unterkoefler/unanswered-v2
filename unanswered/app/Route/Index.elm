module Route.Index exposing (ActionData, Data, Model, Msg, route)

import BackendTask exposing (BackendTask)
import FatalError exposing (FatalError)
import Head
import Head.Seo as Seo
import Pages.Url
import Pages.PageUrl exposing (PageUrl)
import PagesMsg exposing (PagesMsg)
import UrlPath
import Route
import RouteBuilder exposing (App, StatelessRoute, StatefulRoute)
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
import Date
import Utils exposing (..)
import Effect
import Url
import Dict
import Url.Builder
import Pagination


type alias Model =
    { page : Int } -- 0-indexed


type alias Msg =
    ()


type alias RouteParams =
    {}


type alias Data =
    { posts : List Posts.Post
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
            , subscriptions = \_ _ _ _ -> Sub.none
            , update = \app shared msg model -> ( model, Effect.none )
            }

init : App Data ActionData RouteParams -> Shared.Model -> ( Model, Effect.Effect Msg )
init app shared =
    ( { page = pageFromUrl app.url }, Effect.none )

pageFromUrl : Maybe PageUrl -> Int
pageFromUrl url =
    case url of
        Nothing ->
            0
        Just { query } ->
            let
                pagesQueryValue = Dict.get "page" query |> Maybe.withDefault []
            in
            case pagesQueryValue of
                [] ->
                    0
                val :: _ ->
                    -- defaulting to first value if url is like ?page=1&page=2
                    String.toInt val |> Maybe.withDefault 0


data : BackendTask FatalError Data
data =
    BackendTask.succeed Data
        |> BackendTask.andMap
            (BackendTask.map sortPosts Posts.posts)
            

sortPosts : List Posts.Post -> List Posts.Post
sortPosts =
    List.sortBy 
        (\p -> 
            ( Date.year p.date * -1
            , Date.monthNumber p.date * -1
            , Date.day p.date * -1
        ))


head :
    App Data ActionData RouteParams
    -> List Head.Tag
head app =
    seoSummary
        { title = "Unanswered.blog"
        , description = "Your second favorite blog"
        , imageOverride = Nothing
        }
        |> Seo.website


view :
    App Data ActionData RouteParams
    -> Shared.Model
    -> Model
    -> View (PagesMsg Msg)
view app shared model =
    { title = "Unanswered.blog"
    , pageLayout = View.HomePage
    , body =
        postPreviews shared.colorScheme app.data.posts model
    , next = Nothing
    , previous = Nothing
    }



postPreviews : Colors.ColorScheme -> List Posts.Post -> Model -> Element msg
postPreviews colorScheme posts { page } =
    column
        [ Border.widthEach { top = 0, bottom = 0, left = 1, right = 1 }
        , spacing 36
        , width (fill |> maximum 800)
        , paddingXY 24 20
        , centerX
        ]
    <|
        case posts of
            [] ->
                [ paragraph [ Font.italic ] [ text "No posts found" ] ]

            _ ->
                (posts 
                    |> Pagination.items { itemsPerPage = 15, currentPage = page }
                    |> List.map Posts.preview
                ) ++
                [ Pagination.controls 
                    [ Font.color <| Colors.link colorScheme ]
                    { numItems = List.length posts
                    , currentPage = page 
                    , itemsPerPage = itemsPerPage
                    , baseUrl = ""
                    , queryParams = []
                    } 
                ]

itemsPerPage : Int
itemsPerPage =
    15
