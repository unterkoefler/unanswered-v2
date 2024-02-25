module NonBlogPost exposing (NonBlogPost, load, view)

import BackendTask exposing (BackendTask)
import FatalError exposing (FatalError)
import BackendTask.File
import Json.Decode as Decode exposing (Decoder)
import Element exposing (Element)
import Post
import Shared

type alias NonBlogPost =
    { title : String
    , body : String
    }


load : String -> BackendTask FatalError NonBlogPost
load filePath =
    BackendTask.File.bodyWithFrontmatter bodyWithFrontmatterDecoder filePath
        |> BackendTask.allowFatal

bodyWithFrontmatterDecoder : String -> Decoder NonBlogPost
bodyWithFrontmatterDecoder body =
    Decode.map
        (\title -> { title = title, body = body } )
        (Decode.field "title" Decode.string)
        

view : Shared.Model -> NonBlogPost -> Element msg
view shared post =
    Post.view shared { title = post.title, body = post.body, date = Nothing }
