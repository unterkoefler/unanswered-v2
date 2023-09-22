module Posts exposing (Post, PostWithBody, posts, post, preview, view)

import BackendTask exposing (BackendTask)
import BackendTask.Glob as Glob
import BackendTask.File
import Json.Decode as Decode exposing (Decoder)
import FatalError exposing (FatalError)
import Element exposing (..)
import Element.Font as Font
import Element.Region as Region
import Date exposing (Date)
import Route
import UrlPath
import Colors
import Renderer exposing (renderPost)
import Font exposing (fontSize)
import Shared
import Utils exposing (..)
import Post


type alias Post =
    { slug : String
    , title : String
    , date : Date
    , description : String
    }

type alias PostWithBody =
    { metadata : Post
    , body : String
    }

type alias UnreadPost =
    { slug : String
    , date : { year : Int, month : Int, day : Int }
    , filePath : String
    }

posts : BackendTask FatalError (List Post)
posts = 
    unreadPosts
        |> BackendTask.map
            (List.map readPost
            )
        |> BackendTask.resolve

post : String -> BackendTask FatalError PostWithBody
post slug =
    findUnreadPost slug
        |> BackendTask.andThen readPostWithBody

readPost : UnreadPost -> BackendTask FatalError Post
readPost unreadPost =
        BackendTask.File.onlyFrontmatter frontmatterDecoder unreadPost.filePath
            |> BackendTask.map
            (\frontmatter -> 
                let
                    { year, month, day } = unreadPost.date
                in
                { slug = unreadPost.slug
                , date = Date.fromCalendarDate year (Date.numberToMonth month) day
                , title = frontmatter.title
                , description = frontmatter.description
                }
                )
                |> BackendTask.allowFatal

readPostWithBody : UnreadPost -> BackendTask FatalError PostWithBody
readPostWithBody unreadPost =
        BackendTask.File.bodyWithFrontmatter bodyWithFrontmatterDecoder unreadPost.filePath
            |> BackendTask.map
            (\bodyWithFrontmatter -> 
                let
                    { year, month, day } = unreadPost.date
                in
                { body = bodyWithFrontmatter.body
                , metadata = 
                    { slug = unreadPost.slug
                    , date = Date.fromCalendarDate year (Date.numberToMonth month) day
                    , title = bodyWithFrontmatter.title
                    , description = bodyWithFrontmatter.description
                    }
                }
                )
                |> BackendTask.allowFatal

bodyWithFrontmatterDecoder : String -> Decoder { title : String, description : String, body: String }
bodyWithFrontmatterDecoder body =
    Decode.map
        (\{ title, description } -> { title = title, description = description, body = body })
        frontmatterDecoder

frontmatterDecoder : Decoder { title : String, description : String }
frontmatterDecoder =
    Decode.map2 (\title description -> { title = title, description = description })
        (Decode.field "title" Decode.string)
        (Decode.field "description" Decode.string)


unreadPosts: BackendTask FatalError (List UnreadPost)
unreadPosts =
    Glob.succeed
        (\filePath year month day slug -> 
                { slug = slug
                , filePath = filePath
                , date = { year = year, month = month, day = day }
                }
        )
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

findUnreadPost : String -> BackendTask FatalError UnreadPost
findUnreadPost targetSlug =
    Glob.succeed
        (\filePath year month day -> 
                { slug = targetSlug
                , filePath = filePath
                , date = { year = year, month = month, day = day }
                }
        )
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


preview : Post -> Element msg
preview p =
    column
        [ spacing 12 ]
        [ previewTitle p.title p.slug
        , viewDescription p.description
        ]


previewTitle : String -> String -> Element msg
previewTitle title slug =
    link
        [ Font.size 18 ]
        { label = paragraph [] [ text title ]
        , url = Route.Slug_ { slug = slug } |> Route.toPath |> UrlPath.toRelative
        }


viewDescription : String -> Element msg
viewDescription description =
    paragraph
        [ Font.size 14
        , Font.italic
        ]
    <|
        [ text description ] 

view : Shared.Model -> PostWithBody -> Element msg
view sharedModel p =
    Post.view sharedModel { title = p.metadata.title, date = Just p.metadata.date, body = p.body }

