module Api exposing
    ( AuthResult
    , ContinueItem
    , Item
    , authHeader
    , fetchAuth
    , fetchContinueWatching
    , fetchLibrary
    , fetchNew
    , fetchPopular
    , streamUrl
    )

{-| Client for the functonal-media-server HTTP API. Progress reporting
(POST /progress) is done directly from JS in index.html on a timer, not
through Elm, so there's no Elm-side function for it. -}

import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


baseUrl : String
baseUrl =
    "http://localhost:8080"


type alias Item =
    { id : String
    , title : String
    , size : Int
    , mime : String
    }


itemDecoder : Decoder Item
itemDecoder =
    Decode.map4 Item
        (Decode.field "id" Decode.string)
        (Decode.field "title" Decode.string)
        (Decode.field "size" Decode.int)
        (Decode.field "mime" Decode.string)


{-| A library item with an in-progress watch percentage (0..100), as
returned by `/continuewatching`. -}
type alias ContinueItem =
    { id : String
    , title : String
    , size : Int
    , mime : String
    , progress : Int
    }


continueItemDecoder : Decoder ContinueItem
continueItemDecoder =
    Decode.map5 ContinueItem
        (Decode.field "id" Decode.string)
        (Decode.field "title" Decode.string)
        (Decode.field "size" Decode.int)
        (Decode.field "mime" Decode.string)
        (Decode.field "progress" Decode.int)


{-| The auth token returned by POST /Users/AuthenticateByName, plus the
logged-in user's name and admin flag (from the `User` object). -}
type alias AuthResult =
    { token : String
    , userName : String
    , isAdmin : Bool
    }


authResultDecoder : Decoder AuthResult
authResultDecoder =
    Decode.map3 AuthResult
        (Decode.field "AccessToken" Decode.string)
        (Decode.oneOf
            [ Decode.field "User" (Decode.field "Name" Decode.string)
            , Decode.field "User" (Decode.field "name" Decode.string)
            ]
        )
        (Decode.oneOf
            [ Decode.field "User" (Decode.field "IsAdministrator" Decode.bool)
            , Decode.field "User" (Decode.field "isAdministrator" Decode.bool)
            ]
        )


{-| Header sent on every authenticated request (Jellyfin/Emby style).
`Nothing` (open mode / logged out) sends no header.
-}
authHeader : Maybe String -> List Http.Header
authHeader token =
    case token of
        Just t ->
            [ Http.header "X-Emby-Token" t ]

        Nothing ->
            []


fetchAuth : String -> String -> (Result Http.Error AuthResult -> msg) -> Cmd msg
fetchAuth user pass toMsg =
    Http.post
        { url = baseUrl ++ "/Users/AuthenticateByName"
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "Username", Encode.string user )
                    , ( "Pw", Encode.string pass )
                    ]
                )
        , expect = Http.expectJson toMsg authResultDecoder
        }


fetchLibrary : Maybe String -> (Result Http.Error (List Item) -> msg) -> Cmd msg
fetchLibrary token toMsg =
    Http.request
        { method = "GET"
        , headers = authHeader token
        , url = baseUrl ++ "/library"
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg (Decode.list itemDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }


fetchContinueWatching : Maybe String -> (Result Http.Error (List ContinueItem) -> msg) -> Cmd msg
fetchContinueWatching token toMsg =
    Http.request
        { method = "GET"
        , headers = authHeader token
        , url = baseUrl ++ "/continuewatching"
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg (Decode.list continueItemDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }


fetchPopular : Maybe String -> (Result Http.Error (List Item) -> msg) -> Cmd msg
fetchPopular token toMsg =
    Http.request
        { method = "GET"
        , headers = authHeader token
        , url = baseUrl ++ "/popular"
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg (Decode.list itemDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }


fetchNew : Maybe String -> (Result Http.Error (List Item) -> msg) -> Cmd msg
fetchNew token toMsg =
    Http.request
        { method = "GET"
        , headers = authHeader token
        , url = baseUrl ++ "/new"
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg (Decode.list itemDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }


streamUrl : String -> String
streamUrl id =
    baseUrl ++ "/stream/" ++ id
