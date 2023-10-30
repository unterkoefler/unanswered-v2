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
import Date
import Utils exposing (..)


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
    -> View (PagesMsg Msg)
view app shared =
    { title = "Unanswered.blog"
    , pageLayout = View.HomePage
    , body =
        postPreviews app.data.posts shared.width
    , next = Nothing
    , previous = Nothing
    }



postPreviews : List Posts.Post -> Int -> Element msg
postPreviews posts w =
    column
        [ Border.widthEach { top = 0, bottom = 0, left = 1, right = 1 }
        , spacing 36
        , width (fill |> maximum 800)
        , paddingXY  (w * 6 // 100) 20
        , centerX
        ]
    <|
        case posts of
            [] ->
                [ paragraph [ Font.italic ] [ text "No posts found" ] ]

            _ ->
                List.map Posts.preview posts -- TODO: pagination
