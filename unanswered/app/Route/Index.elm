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
    let
        _ = Debug.log "app.url" app.url
    in
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
                    |> List.drop (page * itemsPerPage)
                    |> List.take itemsPerPage
                    |> List.map Posts.preview
                ) ++
                [ paginationControls colorScheme { numPosts = List.length posts, currentPage = page } ]

paginationControls : Colors.ColorScheme -> { numPosts : Int, currentPage : Int } -> Element msg
paginationControls colorScheme { numPosts, currentPage } = 
    let
        numPages : Int
        numPages =
            ceiling (toFloat numPosts / toFloat itemsPerPage)

        currentPageInfo : String
        currentPageInfo =
            "Page " ++ String.fromInt (currentPage + 1) ++ " of " ++ String.fromInt numPages

        hasPrevious : Bool
        hasPrevious =
            currentPage > 0

        hasNext : Bool
        hasNext =
            currentPage < numPages - 1

        previousEl =
            if hasPrevious then
                link
                    [ Font.color <| Colors.link colorScheme 
                    , alignLeft
                    ]
                    { label = text "Previous"
                    , url = Url.Builder.absolute [] [ Url.Builder.int "page" (currentPage - 1) ]
                    }
            else
                Element.none

        nextEl =
            if hasNext then
                link
                    [ Font.color <| Colors.link colorScheme 
                    , alignRight
                    ]
                    { label = text "Next"
                    , url = Url.Builder.absolute [] [ Url.Builder.int "page" (currentPage + 1) ]
                    }
            else
                Element.none
    in
    row
        [ width fill, Font.size 14 ]
        [ previousEl
        , el [ centerX ] <| text currentPageInfo
        , nextEl
        ]

        

itemsPerPage : Int
itemsPerPage =
    15
