module Effect exposing (Effect(..), batch, fromCmd, map, none, perform)

{-|

@docs Effect, batch, fromCmd, map, none, perform

-}

import Browser.Navigation
import Form
import Http
import Json.Decode as Decode
import Pages.Fetcher
import Url exposing (Url)


{-| -}
type Effect msg
    = None
    | Cmd (Cmd msg)
    | Batch (List (Effect msg))
    | GetStargazers (Result Http.Error Int -> msg)
    | SetField { formId : String, name : String, value : String }
    | FetchRouteData
        { data : Maybe FormData
        , toMsg : Result Http.Error Url -> msg
        }
    | Submit
        { values : FormData
        , toMsg : Result Http.Error Url -> msg
        }
    | Subscribe
        { values : { email : String }
        , toMsg : Result Http.Error String -> msg
        }
    | SubmitFetcher (Pages.Fetcher.Fetcher msg)
    | BrowserCmd (Browser.Navigation.Key -> Cmd msg)


{-| -}
type alias RequestInfo =
    { contentType : String
    , body : String
    }


{-| -}
none : Effect msg
none =
    None


{-| -}
batch : List (Effect msg) -> Effect msg
batch =
    Batch


{-| -}
fromCmd : Cmd msg -> Effect msg
fromCmd =
    Cmd


{-| -}
map : (a -> b) -> Effect a -> Effect b
map fn effect =
    case effect of
        None ->
            None

        Cmd cmd ->
            Cmd (Cmd.map fn cmd)

        Batch list ->
            Batch (List.map (map fn) list)

        GetStargazers toMsg ->
            GetStargazers (toMsg >> fn)

        FetchRouteData fetchInfo ->
            FetchRouteData
                { data = fetchInfo.data
                , toMsg = fetchInfo.toMsg >> fn
                }

        Submit fetchInfo ->
            Submit
                { values = fetchInfo.values
                , toMsg = fetchInfo.toMsg >> fn
                }

        Subscribe subscribeInfo ->
            Subscribe
                { values = subscribeInfo.values
                , toMsg = subscribeInfo.toMsg >> fn
                }

        SetField info ->
            SetField info

        SubmitFetcher fetcher ->
            fetcher
                |> Pages.Fetcher.map fn
                |> SubmitFetcher

        BrowserCmd cmdFn -> -- cmdFn : Key -> Cmd a
            BrowserCmd (\key -> Cmd.map fn (cmdFn key))
                    -- return: Key -> Cmd b


{-| -}
perform :
    { fetchRouteData :
        { data : Maybe FormData
        , toMsg : Result Http.Error Url -> pageMsg
        }
        -> Cmd msg
    , submit :
        { values : FormData
        , toMsg : Result Http.Error Url -> pageMsg
        }
        -> Cmd msg
    , runFetcher :
        Pages.Fetcher.Fetcher pageMsg
        -> Cmd msg
    , fromPageMsg : pageMsg -> msg
    , key : Browser.Navigation.Key
    , setField : { formId : String, name : String, value : String } -> Cmd msg
    }
    -> Effect pageMsg
    -> Cmd msg
perform ({ fromPageMsg, key } as helpers) effect =
    case effect of
        None ->
            Cmd.none

        Cmd cmd ->
            Cmd.map fromPageMsg cmd

        SetField info ->
            helpers.setField info

        Batch list ->
            Cmd.batch (List.map (perform helpers) list)

        GetStargazers toMsg ->
            Http.get
                { url =
                    "https://api.github.com/repos/dillonkearns/elm-pages"
                , expect = Http.expectJson (toMsg >> fromPageMsg) (Decode.field "stargazers_count" Decode.int)
                }

        FetchRouteData fetchInfo ->
            helpers.fetchRouteData
                fetchInfo

        Submit record ->
            helpers.submit record

        SubmitFetcher record ->
            helpers.runFetcher record

        Subscribe { values, toMsg } ->
            subscribe values (toMsg >> fromPageMsg)

        BrowserCmd cmdFn ->
            Cmd.map fromPageMsg <| cmdFn key


type alias FormData =
    { fields : List ( String, String )
    , method : Form.Method
    , action : String
    , id : Maybe String
    }

subscribe : { email : String } -> (Result Http.Error String -> msg) -> Cmd msg
subscribe data toMsg = 
    Http.request
        { method = "POST"
        , headers = [] -- TODO?
        , url = "https://buttondown.com/api/emails/embed-subscribe/unterkoefler"
        , body = Http.stringBody "application/x-www-form-urlencoded" (formData data)
        , expect = expect302Redirect toMsg
        , timeout = Nothing
        , tracker = Nothing
        }

expect302Redirect : (Result Http.Error String -> msg) -> Http.Expect msg
expect302Redirect toMsg =
    Http.expectStringResponse toMsg <|
        \response->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)
                Http.Timeout_ ->
                    Err Http.Timeout
                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata body ->
                    Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ metadata _ ->
                    case metadata.statusCode of
                        200 ->
                            Ok metadata.url
                        _ ->
                            Err (Http.BadStatus metadata.statusCode)

formData : { email : String } -> String
formData { email }  =
    "email=" ++ (Url.percentEncode email)
