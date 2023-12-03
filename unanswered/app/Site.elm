module Site exposing (config)

import BackendTask exposing (BackendTask)
import FatalError exposing (FatalError)
import Head
import SiteConfig exposing (SiteConfig)
import MimeType
import Pages.Url as Url
import UrlPath


config : SiteConfig
config =
    { canonicalUrl = "https://v2.unanswered.blog"
    , head = head
    }


head : BackendTask FatalError (List Head.Tag)
head =
    [ Head.metaName "viewport" (Head.raw "width=device-width,initial-scale=1")
    , Head.sitemapLink "/sitemap.xml"
    , Head.icon [ (32, 32) ] MimeType.Png (Url.fromPath (UrlPath.join ["favicon-32x32.png"]))
    , Head.icon [ (16, 16) ] MimeType.Png (Url.fromPath (UrlPath.join ["favicon-16x16.png" ]))
    ]
        |> BackendTask.succeed
