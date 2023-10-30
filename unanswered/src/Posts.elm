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
import PagesMsg


type alias Post =
    { slug : String
    , title : String
    , date : Date
    , description : String
    , next : Maybe String
    , previous : Maybe String
    }

type alias PostWithBody =
    { metadata : Post
    , body : String
    , next : Maybe String
    , previous : Maybe String
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
        |> BackendTask.map
            (List.sortBy
                (\p ->
                    ( Date.year p.date * -1
                    , Date.monthNumber p.date * -1
                    , Date.day p.date * -1
                    )
                )
            )

        |> BackendTask.map populateNextAndPreviousPosts

post : String -> BackendTask FatalError PostWithBody
post slug =
    findUnreadPost slug
        |> BackendTask.andThen readPostWithBody
        |> BackendTask.andThen findPreviousAndNextPosts


findPreviousAndNextPosts : PostWithBody -> BackendTask FatalError PostWithBody
findPreviousAndNextPosts target =
    posts
        |> BackendTask.map
            (findPost target.metadata.slug >> updatePost target)

findPost : String -> List Post -> Maybe Post
findPost slug list =
    case list of
        [] ->
            Nothing
        x::xs ->
            if x.slug == slug then
                Just x
            else
                findPost slug xs

updatePost : PostWithBody -> Maybe Post -> PostWithBody
updatePost postWithBody p =
        { postWithBody 
        | previous = Maybe.map .previous p |> Maybe.withDefault Nothing
        , next = Maybe.map .next p |> Maybe.withDefault Nothing
        }

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
                , next = Nothing
                , previous = Nothing
                }
                )
            |> BackendTask.allowFatal -- TODO: handle recoverable errors? 

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
                    , next = Nothing
                    , previous = Nothing
                    }
                , next = Nothing
                , previous = Nothing 
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

populateNextAndPreviousPosts : List Post -> List Post
populateNextAndPreviousPosts =
    populateNextAndPreviousPostsHelp Nothing []

populateNextAndPreviousPostsHelp : Maybe Post -> List Post -> List Post -> List Post
populateNextAndPreviousPostsHelp prev acc l =
    case l of
        [] ->
            List.reverse acc

        [last] ->
            case prev of
                Nothing ->
                    populateNextAndPreviousPostsHelp
                        (Just last)
                        (last :: acc)
                        []
                Just p ->
                    populateNextAndPreviousPostsHelp
                        (Just last)
                        ({ p | next = Just last.slug } :: { last | previous = Just p.slug } :: acc)
                        []

        curr::rest ->
            case prev of
                Nothing ->
                    populateNextAndPreviousPostsHelp
                        (Just curr)
                        acc
                        rest

                Just p ->
                    populateNextAndPreviousPostsHelp
                        (Just { curr | previous = Just p.slug })
                        ( { p | next = Just curr.slug } :: acc)
                        rest



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

view : Shared.Model -> PostWithBody -> Element msg-- (PagesMsg.PagesMsg Shared.Msg)
view sharedModel p =
    Post.view sharedModel { title = p.metadata.title, date = Just p.metadata.date, body = p.body }

