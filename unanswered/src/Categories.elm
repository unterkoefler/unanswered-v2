module Categories exposing (view)

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


