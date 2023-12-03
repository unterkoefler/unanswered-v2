module Route.Slug_ exposing (ActionData, Data, Model, Msg, route)

import BackendTask exposing (BackendTask)
import BackendTask.Glob as Glob
import BackendTask.File
import FatalError exposing (FatalError)
import Head
import Head.Seo as Seo 
import Element exposing (..)
import Pages.Url
import PagesMsg exposing (PagesMsg)
import RouteBuilder exposing (App, StatelessRoute)
import Shared
import View exposing (View)
import Json.Decode as Decode exposing (Decoder)
import Posts
import Utils exposing (..)
import UrlPath
import MimeType
import LanguageTag.Country as Country
import LanguageTag.Language as Language


type alias Model =
    {}


type alias Msg =
    ()


type alias RouteParams =
    { slug : String 
    }


route : StatelessRoute RouteParams Data ActionData
route =
    RouteBuilder.preRender
        { head = head
        , pages = pages
        , data = data
        }
        |> RouteBuilder.buildNoState { view = view }


pages : BackendTask FatalError (List RouteParams)
pages =
        BackendTask.map (List.map (\post -> { slug = post.slug })) Posts.posts

type alias Data = Posts.PostWithBody

type alias ActionData =
    {}


data : RouteParams -> BackendTask FatalError Data
data routeParams =
    Posts.post routeParams.slug
   

head :
    App Data ActionData RouteParams
    -> List Head.Tag
head app =
    seoSummary
        { imageOverride = Nothing
        , description = seoDescription app.data.metadata.description
        , title = app.data.metadata.title
        }
        |> Seo.website

seoDescription : String -> String
seoDescription desc =
    if String.isEmpty desc then
        "A blog post from your second favorite blog"
    else
        desc


view :
    App Data ActionData RouteParams
    -> Shared.Model
    -> View msg
view app sharedModel =
    { title = "Unanswered.blog - " ++ app.data.metadata.title
    , pageLayout = View.PostPage
    , body = 
        Posts.view 
            sharedModel
            app.data
    , next = app.data.next
    , previous = app.data.previous
    }
