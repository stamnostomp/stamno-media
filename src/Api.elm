module Api exposing
    ( AuthResult
    , ContinueItem
    , CreateRequestPayload
    , DiscoverItem
    , Item
    , Release
    , RequestEntry
    , Tracker
    , authHeader
    , createRequest
    , fetchAuth
    , fetchContinueWatching
    , fetchDiscoverPopular
    , fetchLibrary
    , fetchNew
    , fetchPopular
    , fetchRequests
    , fetchTrackers
    , searchIndexers
    , searchReleases
    , streamUrl
    )

{-| Client for the functonal-media-server HTTP API. Progress reporting
(POST /progress) is done directly from JS in index.html on a timer, not
through Elm, so there's no Elm-side function for it. -}

import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Url


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


{-| One media request from GET /Requests (m20a/m20b backend, PascalCase). -}
type alias RequestEntry =
    { id : String
    , title : String
    , year : Maybe Int
    , mediaType : String
    , status : String
    , sizeBytes : Maybe Int
    , sourceRelease : Maybe String
    , createdAt : String
    }


requestEntryDecoder : Decoder RequestEntry
requestEntryDecoder =
    Decode.map8 RequestEntry
        (Decode.field "Id" Decode.string)
        (Decode.field "Title" Decode.string)
        (Decode.maybe (Decode.field "Year" Decode.int))
        (Decode.field "MediaType" Decode.string)
        (Decode.field "Status" Decode.string)
        (Decode.maybe (Decode.field "SizeBytes" Decode.int))
        (Decode.maybe (Decode.field "SourceRelease" Decode.string))
        (Decode.field "CreatedAt" Decode.string)


{-| A tracker (Jackett indexer) from GET /Trackers. `ApiKey` is null for
non-admins on the server side and is not modelled here. -}
type alias Tracker =
    { id : String
    , name : String
    , url : String
    , enabled : Bool
    }


trackerDecoder : Decoder Tracker
trackerDecoder =
    Decode.map4 Tracker
        (Decode.field "Id" Decode.string)
        (Decode.field "Name" Decode.string)
        (Decode.field "Url" Decode.string)
        (Decode.field "Enabled" Decode.bool)


{-| One Torznab release from GET /Trackers/:id/Search. -}
type alias Release =
    { title : String
    , sizeBytes : Maybe Int
    , seeders : Maybe Int
    , downloadUrl : Maybe String
    , magnet : Maybe String
    , trackerId : String
    }


releaseDecoder : Decoder Release
releaseDecoder =
    Decode.map6 Release
        (Decode.field "Title" Decode.string)
        (Decode.maybe (Decode.field "SizeBytes" Decode.int))
        (Decode.maybe (Decode.field "Seeders" Decode.int))
        (Decode.maybe (Decode.field "DownloadUrl" Decode.string))
        (Decode.maybe (Decode.field "Magnet" Decode.string))
        (Decode.field "TrackerId" Decode.string)


{-| Body of POST /Requests: Title is required, everything else optional. -}
type alias CreateRequestPayload =
    { title : String
    , year : Maybe Int
    , mediaType : Maybe String
    , trackerId : Maybe String
    , release : Maybe String
    , sizeBytes : Maybe Int
    , magnet : Maybe String
    , downloadUrl : Maybe String
    }


encodeMaybe : String -> (a -> Encode.Value) -> Maybe a -> List ( String, Encode.Value )
encodeMaybe key enc maybeVal =
    case maybeVal of
        Just v ->
            [ ( key, enc v ) ]

        Nothing ->
            []


encodeCreateRequest : CreateRequestPayload -> Encode.Value
encodeCreateRequest p =
    Encode.object <|
        [ ( "Title", Encode.string p.title ) ]
            ++ encodeMaybe "Year" Encode.int p.year
            ++ encodeMaybe "Type" Encode.string p.mediaType
            ++ encodeMaybe "TrackerId" Encode.string p.trackerId
            ++ encodeMaybe "Release" Encode.string p.release
            ++ encodeMaybe "SizeBytes" Encode.int p.sizeBytes
            ++ encodeMaybe "Magnet" Encode.string p.magnet
            ++ encodeMaybe "DownloadUrl" Encode.string p.downloadUrl


{-| GET /Requests -> the {"Requests": [...]} envelope (non-admins get only
their own requests). -}
fetchRequests : Maybe String -> (Result Http.Error (List RequestEntry) -> msg) -> Cmd msg
fetchRequests token toMsg =
    Http.request
        { method = "GET"
        , headers = authHeader token
        , url = baseUrl ++ "/Requests"
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg (Decode.field "Requests" (Decode.list requestEntryDecoder))
        , timeout = Nothing
        , tracker = Nothing
        }


{-| GET /Trackers -> the tracker list (ApiKey redacted server-side for
non-admins). -}
fetchTrackers : Maybe String -> (Result Http.Error (List Tracker) -> msg) -> Cmd msg
fetchTrackers token toMsg =
    Http.request
        { method = "GET"
        , headers = authHeader token
        , url = baseUrl ++ "/Trackers"
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg (Decode.list trackerDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }


{-| GET /Trackers/:id/Search?q=..&type=movie -> matching Torznab releases. -}
searchReleases : Maybe String -> String -> String -> (Result Http.Error (List Release) -> msg) -> Cmd msg
searchReleases token trackerId query toMsg =
    Http.request
        { method = "GET"
        , headers = authHeader token
        , url = baseUrl ++ "/Trackers/" ++ trackerId ++ "/Search?q=" ++ Url.percentEncode query ++ "&type=movie"
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg (Decode.list releaseDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }


{-| POST /Requests. The msg carries the HTTP status code alongside the
result so the caller can distinguish 409 (duplicate) from other errors:
`toMsg code result`. -}
createRequest : Maybe String -> CreateRequestPayload -> (Int -> Result Http.Error RequestEntry -> msg) -> Cmd msg
createRequest token payload toMsg =
    Http.request
        { method = "POST"
        , headers = authHeader token
        , url = baseUrl ++ "/Requests"
        , body = Http.jsonBody (encodeCreateRequest payload)
        , expect =
            Http.expectStringResponse
                (\result ->
                    case result of
                        Ok ( code, entry ) ->
                            toMsg code (Ok entry)

                        Err err ->
                            toMsg (errorCodeOf err) (Err err)
                )
                entryResultOf
        , timeout = Nothing
        , tracker = Nothing
        }


errorCodeOf : Http.Error -> Int
errorCodeOf err =
    case err of
        Http.BadStatus code ->
            code

        _ ->
            0


entryResultOf : Http.Response String -> Result Http.Error ( Int, RequestEntry )
entryResultOf resp =
    case resp of
        Http.BadUrl_ u ->
            Err (Http.BadUrl u)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.BadStatus_ md body ->
            if md.statusCode == 200 || md.statusCode == 201 then
                case decodeEntry body of
                    Ok entry ->
                        Ok ( md.statusCode, entry )

                    Err e ->
                        Err e

            else
                Err (Http.BadStatus md.statusCode)

        Http.GoodStatus_ md body ->
            case decodeEntry body of
                Ok entry ->
                    Ok ( md.statusCode, entry )

                Err e ->
                    Err e


decodeEntry : String -> Result Http.Error RequestEntry
decodeEntry body =
    case Decode.decodeString requestEntryDecoder body of
        Ok entry ->
            Ok entry

        Err err ->
            Err (Http.BadBody (Decode.errorToString err))


{-| One entry of GET /Discover/Popular (m26): a TMDB popular movie/show
with a ready-to-use PosterUrl. -}
type alias DiscoverItem =
    { id : String
    , name : String
    , mediaType : String
    , year : Maybe Int
    , overview : Maybe String
    , vote : Maybe Float
    , posterUrl : Maybe String
    }


discoverItemDecoder : Decoder DiscoverItem
discoverItemDecoder =
    Decode.map7 DiscoverItem
        (Decode.field "Id" Decode.string)
        (Decode.field "Name" Decode.string)
        (Decode.field "Type" Decode.string)
        (Decode.maybe (Decode.field "ProductionYear" Decode.int))
        (Decode.maybe (Decode.field "Overview" Decode.string))
        (Decode.maybe (Decode.field "VoteAverage" Decode.float))
        (Decode.maybe (Decode.field "PosterUrl" Decode.string))


{-| GET /Discover/Popular -> (movies, shows) from the {"Movies":..,"Shows":..}
envelope. -}
fetchDiscoverPopular : Maybe String -> (Result Http.Error ( List DiscoverItem, List DiscoverItem ) -> msg) -> Cmd msg
fetchDiscoverPopular token toMsg =
    Http.request
        { method = "GET"
        , headers = authHeader token
        , url = baseUrl ++ "/Discover/Popular"
        , body = Http.emptyBody
        , expect =
            Http.expectJson toMsg
                (Decode.map2 (\m s -> ( m, s ))
                    (Decode.field "Movies" (Decode.list discoverItemDecoder))
                    (Decode.field "Shows" (Decode.list discoverItemDecoder))
                )
        , timeout = Nothing
        , tracker = Nothing
        }


{-| GET /Indexers/Search?q=..&type=movie[&indexerId=..] (m26): with
`Nothing` the server searches ALL enabled indexers; with `Just` only
that one. -}
searchIndexers : Maybe String -> Maybe String -> String -> (Result Http.Error (List Release) -> msg) -> Cmd msg
searchIndexers token mIndexerId query toMsg =
    let
        base =
            "/Indexers/Search?q=" ++ Url.percentEncode query ++ "&type=movie"

        url =
            case mIndexerId of
                Just iid ->
                    base ++ "&indexerId=" ++ Url.percentEncode iid

                Nothing ->
                    base
    in
    Http.request
        { method = "GET"
        , headers = authHeader token
        , url = baseUrl ++ url
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg (Decode.list releaseDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }
