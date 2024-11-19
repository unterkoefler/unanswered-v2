module Route.Subscribe exposing (ActionData, Data, Model, Msg, route)

import NewTab
import BackendTask exposing (BackendTask)
import FatalError exposing (FatalError)
import Head
import Head.Seo as Seo
import Pages.Url
import Pages.PageUrl exposing (PageUrl)
import PagesMsg exposing (PagesMsg)
import UrlPath
import Route
import RouteBuilder exposing (App, StatelessRoute, StatefulRoute)
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
import Effect
import Url
import Dict
import Url.Builder
import Pagination
import Browser.Navigation
import Http


type alias Model =
    { email : String 
    , errorMessage : Maybe String
    , loading : Bool
    }

defaultModel : Model
defaultModel =
    { email = ""
    , errorMessage = Nothing
    , loading = False
    }


type Msg 
    = EmailChanged String
    | Submit
    | Submitted (Result Http.Error String)

type alias RouteParams =
    {}


type alias Data =
    {}


type alias ActionData =
    {}


route : StatefulRoute RouteParams Data ActionData Model Msg
route =
    RouteBuilder.single
        { head = head
        , data = data
        }
        |> RouteBuilder.buildWithLocalState 
            { view = view 
            , init = init
            , subscriptions = \_ _ _ _ -> Sub.none
            , update = update
            }

init : App Data ActionData RouteParams -> Shared.Model -> ( Model, Effect.Effect Msg )
init app shared =
    ( defaultModel, Effect.none )

data : BackendTask FatalError Data
data =
    BackendTask.succeed Data

update : App Data ActionData RouteParams -> Shared.Model -> Msg -> Model -> ( Model, Effect.Effect Msg )
update app shared msg model =
    case msg of
        EmailChanged newEmail ->
            ( { model | email = newEmail }, Effect.none ) 

        Submit ->
            let
                result = parseEmail model.email
            in
            case result of
                Err errorMsg ->
                    ( { model | errorMessage = Just errorMsg }, Effect.none )

                Ok email ->
                    ( { model | errorMessage = Nothing, loading = True }
                    , Effect.Subscribe { toMsg = Submitted, values = { email = model.email } } 
                    )

        Submitted result ->
            case result of
                Ok url ->
                    ( { model | loading = False }, Effect.fromCmd <| NewTab.newTab url )
                Err httpError ->
                    ( { model | loading = False, errorMessage = Just <| httpErrorToString httpError }
                    , Effect.none 
                    )

httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl s ->
            "Inexplicable! Error code 314-C: " ++ s

        Http.Timeout ->
            "Something went too slow. It wouldn't hurt to try again, I guess"
    
        Http.NetworkError ->
            "That didn't work because something is wrong with your network. It's not my fault."

        Http.BadStatus status ->
            "Oopsies! Request failed with response code " ++ (String.fromInt status)

        Http.BadBody details ->
            "Sheesh. That didn't work and here's why: " ++ details

        

parseEmail : String -> Result String String
parseEmail value =
    if String.isEmpty value then
        Err "Your email cannot be blank. That wouldn't work."
    else if not (String.contains "@" value) then
        Err "You entered an email without an '@' symbol. That's not an email!"
    else if (String.length value) > 254 then
        Err "Your email is really long. Too long, in fact."
    else
        Ok value


head :
    App Data ActionData RouteParams
    -> List Head.Tag
head app =
    seoSummary
        { imageOverride = Nothing
        , description = "Subscription form for the blog"
        , title = "Subscribe"
        }
        |> Seo.website


view :
    App Data ActionData RouteParams
    -> Shared.Model
    -> Model
    -> View (PagesMsg Msg)
view app shared model =
    { title = "Unanswered.blog"
    , pageLayout = View.HomePage
    , body =
        body shared.colorScheme model
    , next = Nothing
    , previous = Nothing
    }

body : Colors.ColorScheme -> Model -> Element (PagesMsg Msg)
body colorScheme model =
    column
        [ spacing 24
        , width (fill |> maximum 800)
        , centerX
        ]
        [ preamble colorScheme
        , form colorScheme model
        ]

form : Colors.ColorScheme -> Model -> Element (PagesMsg Msg)
form colorScheme model =
    let
        (buttonLabel, onPress) = 
            if model.loading then
                ("Subscribing...", Nothing)
            else
                ("Subscribe", Just (Submit |> PagesMsg.fromMsg))
    in
    column
        [ spacing 8
        , width fill
        ]
        [ Input.email
            [ Input.focusedOnLoad
            , Font.color <| Colors.primary colorScheme
            , Background.color <| Colors.secondary colorScheme
            ]
            { label = Input.labelAbove [] <| text "Enter your email"
            , placeholder = Just <| Input.placeholder [] <| text "your@email.com"
            , text = model.email
            , onChange = EmailChanged >> PagesMsg.fromMsg
            }
        , Input.button
            [ Border.rounded 12
            , Background.color (Colors.accent colorScheme)
            , Font.color Colors.white
            , padding 12
            , width fill
            , Font.center
            ]
            { label = text buttonLabel
            , onPress = onPress
            }
        , viewError colorScheme model
        ] 

preamble : Colors.ColorScheme -> Element msg
preamble colorScheme = 
    column
        [ width fill
        , spacing 16
        ]
        [ paragraph
            []
            [ text "This is the email list for my blog, where I type and scream my thoughts (and stories, poems, etc) into the void. Become a member of the void today!"
            ]
        , paragraph
            []
            [ text "I'll send you an email any time there's a new post, which happens sporadically, but usually not more than once a week."
            ]
        , paragraph
            []
            [ text "After you click that big button down there, if everything goes smoothly, you'll be redirected to a confirmation page over at buttondown.com. "
            , link 
                [ Font.color <| Colors.link colorScheme ]
                { url = "https://buttondown.com/refer/unterkoefler"
                , label = text "Buttondown"
                }
            , text " is the email platform I use to manage subscriptions. You'll see a confirmation email from them soon."
            ]
        ]

viewError : Colors.ColorScheme -> Model -> Element (PagesMsg Msg)
viewError colorScheme model =
    case model.errorMessage of
        Nothing ->
            Element.none

        Just message ->
            column 
                [ spacing 24 
                , width fill 
                ]
                [ paragraph
                    [ width fill 
                    , Font.color <| Colors.error colorScheme
                    ]
                    [ text message ]
                , paragraph
                    [ width fill ]
                    [ text "If this thing ain't working too good, you could try subscribing directly at the source - "
                    , newTabLink
                        [ Font.color <| Colors.link colorScheme
                        ]
                        { url = "https://buttondown.com/unterkoefler"
                        , label = text "https://buttondown.com/unterkoefler"
                        }
                    , text ". Cheers!"
                    ]
                    ]

