module Route.Search exposing (ActionData, Data, Model, Msg, route)

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
import Browser.Navigation


type alias Model =
    { page : Int -- 0-indexed
    , searchTerm : String
    , searchFullText : Bool
    }

defaultModel : Model
defaultModel =
    { page = 0
    , searchTerm = ""
    , searchFullText = False
    }


type Msg 
    = SearchChanged String
    | ToggleFullTextSearch Bool


type alias RouteParams =
    {}


type alias Data =
    { posts : List Posts.PostWithBody
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
            , update = update
            }

init : App Data ActionData RouteParams -> Shared.Model -> ( Model, Effect.Effect Msg )
init app shared =
    ( modelFromUrl app.url, Effect.none )

modelFromUrl : Maybe PageUrl -> Model
modelFromUrl url =
    case url of
        Nothing ->
            defaultModel
        Just { query } ->
            let
                page = firstOrDefault (Dict.get "page" query) "0"
                searchTerm = firstOrDefault (Dict.get "s" query) ""
                searchFullText = firstOrDefault (Dict.get "full" query) "False"
            in
            { page = String.toInt page |> Maybe.withDefault 0
            , searchTerm = searchTerm
            , searchFullText = searchFullText == "True"
            }

firstOrDefault : Maybe (List String) -> String -> String
firstOrDefault xs default =
    xs
        |> Maybe.map List.head
        |> Maybe.map (Maybe.withDefault default)
        |> Maybe.withDefault default

data : BackendTask FatalError Data
data =
    BackendTask.succeed Data
        |> BackendTask.andMap
            (BackendTask.map sortPosts Posts.postsWithBodies)
            

sortPosts : List Posts.PostWithBody -> List Posts.PostWithBody
sortPosts =
    List.sortBy 
        (\p -> 
            ( Date.year p.metadata.date * -1
            , Date.monthNumber p.metadata.date * -1
            , Date.day p.metadata.date * -1
        ))

update : App Data ActionData RouteParams -> Shared.Model -> Msg -> Model -> ( Model, Effect.Effect msg )
update app shared msg model =
    case msg of
        SearchChanged term ->
            ( { model | searchTerm = term, page = 0 }
            , Effect.BrowserCmd (\key -> 
                    Browser.Navigation.replaceUrl key 
                        (Url.Builder.absolute 
                            [ "search" ] 
                            [ Url.Builder.string "s" term
                            , Url.Builder.string "full" (boolToStr model.searchFullText)
                            ]
                        )
                    )
            )

        ToggleFullTextSearch newValue ->
            ( { model | searchFullText = newValue, page = 0 }
            , Effect.BrowserCmd (\key -> 
                Browser.Navigation.replaceUrl key 
                    (Url.Builder.absolute [ "search" ]
                        [ Url.Builder.string "s" model.searchTerm
                        , Url.Builder.string "full" (boolToStr newValue)
                        ]
                    )
                )
            )

boolToStr : Bool -> String
boolToStr b =
    if b then "True" else "False"


head :
    App Data ActionData RouteParams
    -> List Head.Tag
head app =
    seoSummary
        { imageOverride = Nothing
        , description = "Search the blog!"
        , title = "Search"
        }
        |> Seo.website


view :
    App Data ActionData RouteParams
    -> Shared.Model
    -> Model
    -> View (PagesMsg Msg)
view app shared model =
    let
        matchedPosts = List.filter (matchesTitle model.searchTerm model.searchFullText) app.data.posts
    in
    { title = "Unanswered.blog"
    , pageLayout = View.HomePage
    , body =
        postPreviews shared.colorScheme matchedPosts model
    , next = Nothing
    , previous = Nothing
    }



postPreviews : Colors.ColorScheme -> List Posts.PostWithBody -> Model -> Element (PagesMsg Msg)
postPreviews colorScheme posts { page, searchTerm, searchFullText } =
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
                [ searchUI searchTerm searchFullText
                , paragraph [ Font.italic ] [ text "No posts found" ] 
                ]

            _ ->
                (searchUI searchTerm searchFullText ::
                (posts 
                    |> Pagination.items { itemsPerPage = itemsPerPage, currentPage = page }
                    |> List.map .metadata
                    |> List.map Posts.preview
                ))++
                [ Pagination.controls 
                    [ Font.color <| Colors.link colorScheme ]
                    { numItems = List.length posts
                    , currentPage = page 
                    , itemsPerPage = itemsPerPage
                    , baseUrl = "search"
                    , queryParams = [ Url.Builder.string "s" searchTerm, Url.Builder.string "full" (boolToStr searchFullText) ]
                    } 
                ]

itemsPerPage : Int
itemsPerPage =
    15


matchesTitle : String -> Bool -> Posts.PostWithBody -> Bool
matchesTitle searchTerm searchFullText post = 
    let
        needle =
            searchTerm |> String.trim |> String.toLower

        haystack =
            (if searchFullText then 
                post.metadata.title ++ " " ++ post.metadata.description ++ " " ++ post.body
            else
                post.metadata.title ++ " " ++ post.metadata.description
            )
                |> String.trim
                |> String.toLower
    in
    case searchTerm of
        "" ->
            True

        _ ->
            String.contains needle haystack


searchUI : String -> Bool -> Element (PagesMsg Msg)
searchUI searchTerm searchFullText =
    column
        [ width fill
        , spacing 12
        ]
        [ Input.text
            [ width fill
            , Input.focusedOnLoad
            ]
            { label = Input.labelAbove [] <| text "Search the blog:"
            , placeholder = Just <| Input.placeholder [] <| text "What dost thou seeketh?"
            , text = searchTerm
            , onChange = SearchChanged >> PagesMsg.fromMsg
            }
        , Input.checkbox
            []
            { onChange = ToggleFullTextSearch >> PagesMsg.fromMsg 
            , icon = Input.defaultCheckbox
            , checked = searchFullText
            , label = Input.labelRight [ Font.size 14 ] <| text "search full text"
            }
        ]
