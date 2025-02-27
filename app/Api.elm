module Api exposing (routes)

import ApiRoute exposing (ApiRoute)
import BackendTask exposing (BackendTask)
import FatalError exposing (FatalError)
import Head
import Html exposing (Html)
import Pages.Manifest as Manifest
import Pages
import Posts
import Route exposing (Route)
import Rss
import Site
import Time


routes :
    BackendTask FatalError (List Route)
    -> (Maybe { indent : Int, newLines : Bool } -> Html Never -> String)
    -> List (ApiRoute ApiRoute.Response)
routes getStaticRoutes htmlToString =
    [ rss
        { siteTagline = ""
        , siteUrl = Site.config.canonicalUrl
        , title = "Unanswered.blog"
        , builtAt = Pages.builtAt
        , indexPage = []
        }
        postsBackendTask
    ]

postsBackendTask : BackendTask FatalError (List Rss.Item)
postsBackendTask = 
    Posts.posts
        |> BackendTask.map
            (List.map
                (\(post) ->
                    { title = post.title
                    , description = post.description
                    , url = post.slug
                    , categories = []
                    , author = "Willy Unterkoefler"
                    , pubDate = Rss.Date post.date
                    , content = Nothing
                    , contentEncoded = Nothing
                    , enclosure = Nothing
                    }
                )
            )


rss : 
    { siteTagline : String
    , siteUrl : String
    , title : String
    , builtAt : Time.Posix
    , indexPage : List String
    }
    -> BackendTask FatalError (List Rss.Item)
    -> ApiRoute.ApiRoute ApiRoute.Response
rss options itemsRequest =
    ApiRoute.succeed
        (itemsRequest
            |> BackendTask.map
                (\items ->
                    Rss.generate
                        { title = options.title
                        , description = options.siteTagline
                        , url = options.siteUrl ++ "/" ++ String.join "/" options.indexPage
                        , lastBuildTime = options.builtAt
                        , generator = Just "elm-pages"
                        , items = items
                        , siteUrl = options.siteUrl
                        }
                )
        )
        |> ApiRoute.literal "feed.xml"
        |> ApiRoute.single
        |> ApiRoute.withGlobalHeadTags
            (BackendTask.succeed
                [ Head.rssLink "/feed.xml"
                ]
            )


manifest : Manifest.Config
manifest =
    Manifest.init
        { name = "Site Name"
        , description = "Description"
        , startUrl = Route.Index |> Route.toPath
        , icons = []
        }
