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
import Categories exposing (Category, categories, CategoryMembers(..), Slug)


type alias Model =
    {}


type alias Msg =
    ()


type alias RouteParams =
    { splat : ( String, List String ) }


type alias Data =
    { category : Category
    , posts : List Posts.Post
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
data { splat } =
    let
        (first, rest ) = splat
        lastSlug = 
            case rest of
                [] ->
                    first
                x::xs ->
                    x -- TODO: hack because max depth is 2 rn

        category = Categories.fromSlug lastSlug
    in
    category
        |> Result.fromMaybe (FatalError.fromString "category not found")
        |> BackendTask.fromResult
        |> BackendTask.andThen categoryAndPostFromCategory

categoryAndPostFromCategory : Category -> BackendTask FatalError Data
categoryAndPostFromCategory category =
    let
        postSlugs = leaves category
        postsWithBodies = List.map Posts.post postSlugs
        posts = List.map (BackendTask.map .metadata) postsWithBodies
    in
    posts
        |> BackendTask.combine
        |> BackendTask.map (\ps -> { category = category, posts = ps })


leaves : Category -> List Slug
leaves category =
    case category.members of
        Slugs { slugs } ->
            slugs

        SubCategories { subCategories } ->
            List.concatMap leaves subCategories
    

head :
    App Data ActionData RouteParams
    -> List Head.Tag
head app =
    seoSummary
        { description = "Blog posts by category" -- TODO: more specific
        , title = "Categories"
        , imageOverride = Nothing
        }
        |> Seo.website

view :
    App Data ActionData RouteParams
    -> Shared.Model
    -> View (PagesMsg Msg)
view app shared =
    { title = "Unanswered.blog - Categories"
    , pageLayout = View.HomePage
    , body =
        Categories.viewCategory shared.colorScheme app.data.category app.data.posts
    , next = Nothing
    , previous = Nothing
    }

