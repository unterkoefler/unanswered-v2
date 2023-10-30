module Route.Category.SPLAT_ exposing (ActionData, Data, Model, Msg, route)

import BackendTask exposing (BackendTask)
import FatalError exposing (FatalError)
import Head
import Head.Seo as Seo
import Pages.Url
import PagesMsg exposing (PagesMsg)
import UrlPath
import Route
import RouteBuilder exposing (App, StatelessRoute)
import Shared
import View exposing (View)
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


type alias Model =
    {}


type alias Msg =
    ()


type alias RouteParams =
    { splat : ( String, List String ) }


type alias Data =
    { 
    }


type alias ActionData =
    {}


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
    BackendTask.succeed (List.concatMap categoryToRoutes categories)

categoryToRoutes : Category -> List RouteParams
categoryToRoutes cat =
    { splat = (cat.slug, [])}
    :: subCategoryToRoutes cat.slug cat.members

subCategoryToRoutes : Slug -> CategoryMembers-> List RouteParams
subCategoryToRoutes start members =
    case members of
        Slugs _ ->
            []

        SubCategories { subCategories } ->
            List.map 
                (\subCategory -> { splat = (start, [ subCategory.slug ] ) } )
                subCategories


data : RouteParams -> BackendTask FatalError Data
data _ =
    BackendTask.succeed Data
            

head :
    App Data ActionData RouteParams
    -> List Head.Tag
head app =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = "elm-pages"
        , image =
            { url = [ "images", "icon-png.png" ] |> UrlPath.join |> Pages.Url.fromPath
            , alt = "elm-pages logo"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = "Welcome to elm-pages!"
        , locale = Nothing
        , title = "elm-pages is running"
        }
        |> Seo.website

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
                                    ]
                                }
                      }
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
                    [ "xcuseme"
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


view2 : Colors.ColorScheme -> Element msg
view2 colorScheme =
    column
        [ spacing 24
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
            (\cat -> viewCategoryHelp colorScheme cat 0 False)
            categories
        )


viewCategory : Colors.ColorScheme -> Category -> Element msg
viewCategory colorScheme category =
    column
        [ spacing 24 ]
        [ viewBreadcrumbs colorScheme category
        , viewCategoryHelp colorScheme category 0 True
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


viewCategoryHelp : Colors.ColorScheme -> Category -> Int -> Bool -> Element msg
viewCategoryHelp colorScheme category depth showPosts =
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
                , url = "/category/" ++ category.slug
                }
            ]
        , viewMembers colorScheme category.members depth showPosts
        ]


viewMembers : Colors.ColorScheme -> CategoryMembers -> Int -> Bool -> Element msg
viewMembers colorScheme members depth showPosts =
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
                    List.map viewSlug slugs

                else
                    [ Element.none ]

            SubCategories { subCategories } ->
                List.map
                    (\cat -> viewCategoryHelp colorScheme cat (depth + 1) showPosts)
                    subCategories
        )


viewSlug : Slug -> Element msg
viewSlug slug =
    text slug
    --case Post.fromSlug slug Post.all of
    --    Nothing ->
    --        text ("wrong slug! " ++ slug)

    --    Just post ->
    --        el [ paddingXY 0 12 ] <|
    --            Posts.preview slug post

view :
    App Data ActionData RouteParams
    -> Shared.Model
    -> View (PagesMsg Msg)
view app shared =
    { title = "Unanswered.blog - Categories"
    , pageLayout = View.HomePage
    , body =
        view2 shared.colorScheme
    , next = Nothing
    , previous = Nothing
    }

