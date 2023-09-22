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
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = "elm-pages"
        , image =
            { url = Pages.Url.external "TODO"
            , alt = "elm-pages logo"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = "TODO"
        , locale = Nothing
        , title = "TODO title" -- metadata.title -- TODO
        }
        |> Seo.website


view :
    App Data ActionData RouteParams
    -> Shared.Model
    -> View (PagesMsg Msg)
view app sharedModel =
    { title = "Unanswered.blog - " ++ app.data.metadata.title
    , pageLayout = View.PostPage
    , body = 
        Posts.view 
            sharedModel
            app.data
    }
