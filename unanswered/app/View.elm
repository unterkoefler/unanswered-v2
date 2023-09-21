module View exposing (View, map, PageLayout(..))

{-|

@docs View, map

-}

import Element exposing (Element)


{-| -}
type alias View msg =
    { title : String
    , body : Element msg
    , pageLayout : PageLayout
    }

type PageLayout
    = PostPage
    | HomePage


{-| -}
map : (msg1 -> msg2) -> View msg1 -> View msg2
map fn doc =
    { title = doc.title
    , body = Element.map fn doc.body
    , pageLayout = doc.pageLayout
    }
