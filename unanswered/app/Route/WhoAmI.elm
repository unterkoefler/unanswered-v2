module Route.WhoAmI exposing (ActionData, Data, Model, Msg, route)

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
import NonBlogPost exposing (NonBlogPost)


type alias Model =
    {}


type alias Msg =
    ()


type alias RouteParams =
    {}


type alias Data =
    { post : NonBlogPost
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
    BackendTask.map Data
        (NonBlogPost.load "content/who.md")
            

head :
    App Data ActionData RouteParams
    -> List Head.Tag
head app =
    seoSummary
        { title = "Who am I"
        , description = "A little bit about the author"
        , imageOverride = Nothing
        }
        |> Seo.website


view :
    App Data ActionData RouteParams
    -> Shared.Model
    -> View (PagesMsg Msg)
view app shared =
    { title = "Unanswered.blog - WhoAmI"
    , pageLayout = View.HomePage
    , body =
        NonBlogPost.view shared app.data.post
    , next = Nothing
    , previous = Nothing
    }

