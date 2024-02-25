module Categories exposing (view, Category, CategoryMembers(..), Slug, categories, fromSlug, viewCategory)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
import Element.Input as Input
import Colors
import Posts
import Date
import Utils exposing (..)
import Route exposing (Route)
import UrlPath exposing (UrlPath)
import Posts

fromSlug : Slug -> Maybe Category
fromSlug slug =
    let
        { result } =
            find categories slug
    in
    result


find : List Category -> Slug -> { result : Maybe Category, path : List Slug }
find cats slug =
    case cats of
        [] ->
            { result = Nothing, path = [] }

        first :: rest ->
            if first.slug == slug then
                { result = Just first, path = [ slug ] }

            else
                case first.members of
                    Slugs _ ->
                        find rest slug

                    SubCategories { subCategories } ->
                        let
                            { result, path } =
                                find subCategories slug
                        in
                        case result of
                            Nothing ->
                                find rest slug

                            Just cat ->
                                { result = Just cat, path = first.slug :: path }


breadcrumbs : Category -> List Slug
breadcrumbs category =
    let
        { path } =
            find categories category.slug
    in
    path



type alias Category =
    { name : String
    , slug : Slug
    , members : CategoryMembers
    }


type CategoryMembers
    = Slugs { slugs : List Slug }
    | SubCategories { subCategories : List Category }


type alias Slug =
    String

categories : List Category
categories =
    [ { name = "Fiction"
      , slug = "fiction"
      , members =
            SubCategories
                { subCategories =
                    [ { name = "Fables"
                      , slug = "fables"
                      , members =
                            Slugs
                                { slugs =
                                    [ "pete"
                                    , "orange"
                                    , "squirrel"
                                    , "vultures-envision-a-toaster"
                                    , "christmas-tree"
                                    ]
                                }
                      }
                    , { name = "Horror"
                      , slug = "horror"
                      , members =
                            Slugs
                                { slugs =
                                    [ "zip"
                                    , "two-ways-out"
                                    , "flicker"
                                    , "itch"
                                    , "yoga"
                                    ]
                                }
                      }
                    , { name = "Romance"
                      , slug = "romance"
                      , members =
                            Slugs
                                { slugs =
                                    [ "the-hearse"
                                    , "crust"
                                    , "metaphor"
                                    , "arnold"
                                    , "batman"
                                    ]
                                }
                      }
                    , { name = "Miscellaneous Fiction"
                      , slug = "misc-fiction"
                      , members =
                            Slugs
                                { slugs =
                                    [ "a-dangerous-hobby"
                                    , "bells"
                                    , "hell"
                                    , "hooked-mystery"
                                    , "hooked-solution"
                                    , "aliens"
                                    , "the-tide-rises"
                                    , "false-starts"
                                    ]
                                }
                      }
                    ]
                }
      }
    , { name = "Poems"
      , slug = "poems"
      , members = 
            Slugs
                { slugs = 
                    [ "weighting"
                    ]
                }
      }
    , { name = "News"
      , slug = "news"
      , members =
            Slugs
                { slugs =
                    [ "bipartisan"
                    , "hope-and-upsilon"
                    , "kkkfc-chicken"
                    , "helmet-salad"
                    , "cleveland"
                    , "taken4"
                    ]
                }
      }
    , { name = "Life Updates"
      , slug = "life"
      , members =
            Slugs
                { slugs =
                    [ "new-york"
                    , "xcuseme"
                    , "europe"
                    , "stupid-convos-1"
                    , "stupid-convos-2"
                    , "facebook-radicalized-me"
                    , "modern-commerce"
                    , "silly-hat-ceremony"
                    , "rambling-1"
                    , "raspberries"
                    , "colonial-woman"
                    , "fedex"
                    ]
                }
      }
    , { name = "Opinion"
      , slug = "opinion"
      , members =
            Slugs
                { slugs =
                    [ "lost-and-not-found"
                    , "think-like-a-squirrel"
                    , "houseplant"
                    , "beantown"
                    , "the-boys-and-amazon"
                    , "tech-support"
                    , "four-stars"
                    , "nest"
                    , "aristotle"
                    ]
                }
      }
    , { name = "Clickbait"
      , slug = "clickbait"
      , members =
            Slugs
                { slugs =
                    [ "goose-question"
                    , "simple-trick"
                    ]
                }
      }
    , { name = "Recipes"
      , slug = "recipes"
      , members =
            Slugs
                { slugs =
                    [ "recipe"
                    ]
                }
      }
    , { name = "Ads"
      , slug = "ads"
      , members =
            Slugs
                { slugs =
                    [ "room-for-let"
                    ]
                }
      }
    , { name = "Podcasts"
      , slug = "podcasts"
      , members =
            Slugs
                { slugs =
                    [ "silent-podcast"
                    ]
                }
      }
    , { name = "Legal Documents (Private)"
      , slug = "legal"
      , members =
            Slugs
                { slugs =
                    [ "roommate-agreement"
                    ]
                }
      }
    ]




view : Colors.ColorScheme -> Element msg
view colorScheme =
    column
        [ spacing 24
        , paddingXY 48 0
        ]
        [ heading
        , viewCategories colorScheme
        ]


heading : Element msg
heading =
    paragraph
        []
        [ text "Browse posts by category"
        ]


viewCategories : Colors.ColorScheme -> Element msg
viewCategories colorScheme =
    column
        [ spacing 12
        ]
        (List.map
            (\cat -> viewCategoryHelp colorScheme cat 0 [] False []) 
            categories
        )


viewCategory : Colors.ColorScheme -> Category -> List Posts.Post -> Element msg
viewCategory colorScheme category posts =
    column
        [ spacing 24
        , paddingXY 48 0
        ]
        [ viewBreadcrumbs colorScheme category
        , viewCategoryHelp colorScheme category 0 [] True posts
        ]


viewBreadcrumbs : Colors.ColorScheme -> Category -> Element msg
viewBreadcrumbs colorScheme category =
    paragraph
        []
        (viewBreadcrumb colorScheme "all" "/categories"
            :: (breadcrumbs category
                    |> List.map (\slug -> viewBreadcrumb colorScheme slug ("/category/" ++ slug))
               )
            |> List.intersperse (text ":: ")
        )


viewBreadcrumb : Colors.ColorScheme -> String -> String -> Element msg
viewBreadcrumb colorScheme name url =
    row
        [ paddingEach
            { left = 0
            , right = 6
            , top = 0
            , bottom = 0
            }
        ]
        [ link
            [ Font.color <| Colors.link colorScheme ]
            { label = text name
            , url = url
            }
        ]


viewCategoryHelp : Colors.ColorScheme -> Category -> Int -> List Slug -> Bool -> List Posts.Post -> Element msg
viewCategoryHelp colorScheme category depth path showPosts posts =
    let
        splat : ( String, List String )
        splat =
            case path of
                [] ->
                    (category.slug, [])

                first::rest ->
                    (first, rest ++ [category.slug])
    in
    column
        [ spacing 6
        ]
        [ paragraph
            [ Font.size (18 - (depth * 4))
            , Region.heading (2 + depth)
            ]
            [ link
                [ Font.color <| Colors.link colorScheme
                , Font.underline
                ]
                { label = text category.name
                , url = Route.Category__SPLAT_ { splat = splat } |> Route.toPath |> UrlPath.toAbsolute
                }
            ]
        , viewMembers colorScheme category.members depth (category.slug :: path) showPosts posts
        ]


viewMembers : Colors.ColorScheme -> CategoryMembers -> Int -> List Slug -> Bool -> List Posts.Post -> Element msg
viewMembers colorScheme members depth path showPosts posts =
    column
        [ spacing 8
        , paddingEach
            { left = 18
            , right = 0
            , top = 4
            , bottom = 4
            }
        ]
        (case members of
            Slugs { slugs } ->
                if showPosts then
                    List.map (viewSlug posts) slugs

                else
                    [ Element.none ]

            SubCategories { subCategories } ->
                List.map
                    (\cat -> viewCategoryHelp colorScheme cat (depth + 1) path showPosts posts)
                    subCategories
        )


viewSlug : List Posts.Post -> Slug -> Element msg
viewSlug posts slug =
    let
        maybePost = List.filter (\p -> p.slug == slug) posts |> List.head
        -- TODO: make it so that there's no Maybe here
    in
    case maybePost of
        Nothing ->
            text ("wrong slug! " ++ slug)

        Just post ->
            el [ paddingXY 0 12 ] <|
                Posts.preview post


