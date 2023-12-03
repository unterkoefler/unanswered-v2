module Utils exposing (borderBetween, borderBetweenRow, directions0, seoSummary)

import Element exposing (..)
import Element.Border as Border
import Url exposing (Url)
import Head.Seo as Seo
import MimeType
import LanguageTag.Country as Country
import LanguageTag.Language as Language
import Pages.Url
import UrlPath

seoSummary : { imageOverride: Maybe Seo.Image, description : String, title : String } -> Seo.Common
seoSummary { imageOverride, description, title } = 
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = "unanswered"
        , image = -- TODO: make it so that a post can have a unique image
            { url = ["assets", "default-image.png"] |> UrlPath.join |> Pages.Url.fromPath
            , alt = "unanswered.blog logo"
            , dimensions = Just { width = 365, height = 149 }
            , mimeType = Just <| MimeType.Image MimeType.Png
            }
        , description = description
        , locale = Just (Language.en, Country.us)
        , title = title
        }



directions0 =
    { left = 0, right = 0, top = 0, bottom = 0 }


borderBetween : List (Element msg) -> List (Element msg)
borderBetween elements =
    case elements of
        [] ->
            []

        [ element ] ->
            [ element ]

        element :: rest ->
            el [ Border.widthEach { directions0 | bottom = 1 }, width fill ]
                element
                :: borderBetween rest


borderBetweenRow : List (Element msg) -> List (Element msg)
borderBetweenRow elements =
    case elements of
        [] ->
            []

        [ element ] ->
            [ element ]

        element :: rest ->
            el [ Border.widthEach { directions0 | right = 1 }, paddingEach { directions0 | right = 8 } ]
                element
                :: borderBetweenRow rest
