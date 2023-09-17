module Route.Blog.Slug_ exposing (ActionData, Data, Model, Msg, route)

import BackendTask exposing (BackendTask)
import BackendTask.Glob as Glob
import BackendTask.File
import FatalError exposing (FatalError)
import Head
import Head.Seo as Seo
import Element
import Pages.Url
import PagesMsg exposing (PagesMsg)
import RouteBuilder exposing (App, StatelessRoute)
import Shared
import View exposing (View)
import Json.Decode as Decode exposing (Decoder)


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
        BackendTask.map (List.map (\post -> { slug = post.slug })) blogPosts

blogPosts : BackendTask FatalError (List BlogPost)
blogPosts =
    Glob.succeed
        (\filePath year month date slug -> { slug = slug, filePath = filePath })
        |> Glob.captureFilePath
        |> Glob.match (Glob.literal "content/")
        |> Glob.match (Glob.literal "posts/")
        |> Glob.capture Glob.int
        |> Glob.match (Glob.literal "-")
        |> Glob.capture Glob.int
        |> Glob.match (Glob.literal "-")
        |> Glob.capture Glob.int
        |> Glob.match (Glob.literal "-")
        |> Glob.capture (Glob.wildcard)
        |> Glob.match (Glob.literal ".md")
        |> Glob.toBackendTask

blogPost : String -> BackendTask FatalError BlogPost
blogPost targetSlug =
    Glob.succeed
        (\filePath year month date -> { slug = targetSlug, filePath = filePath })
        |> Glob.captureFilePath
        |> Glob.match (Glob.literal "content/")
        |> Glob.match (Glob.literal "posts/")
        |> Glob.capture Glob.int
        |> Glob.match (Glob.literal "-")
        |> Glob.capture Glob.int
        |> Glob.match (Glob.literal "-")
        |> Glob.capture Glob.int
        |> Glob.match (Glob.literal "-")
        |> Glob.match (Glob.literal targetSlug)
        |> Glob.match (Glob.literal ".md")
        |> Glob.expectUniqueMatch
        |> BackendTask.allowFatal


type alias Data =
    { body : String
    , title : String
    }

type alias BlogPost =
    { slug : String
    , filePath : String
    }


type alias ActionData =
    {}


data : RouteParams -> BackendTask FatalError Data
data routeParams =
    blogPost routeParams.slug
        |> BackendTask.andThen
            (\post ->
                BackendTask.File.bodyWithFrontmatter blogPostDecoder post.filePath 
                    |> BackendTask.allowFatal
            )
 
   
blogPostDecoder : String -> Decoder Data
blogPostDecoder body =
    Decode.map (Data body)
        (Decode.field "title" Decode.string)


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
    { title = "Placeholder - Blog.Slug_"
    , body = 
        Element.column
            []
            [ Element.text app.data.title 
            , Element.paragraph []
                [ Element.text app.data.body ]
            ]
    }
