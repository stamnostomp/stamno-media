module Api exposing
    ( AuthResult
    , baseUrl
    , ContinueItem
    , CreateRequestPayload
    , DiscoverItem
    , Item
    , ItemMetadata
    , PlaybackInfo
    , Release
    , RequestEntry
    , SeriesDetail
    , SeriesEpisode
    , SeriesHit
    , SeriesSeason
    , Tracker
    , discoverItemDecoder
    , qParams
    , releaseDecoder
    , searchResultsDecoder
    , seriesDetailDecoder
    , seriesHitDecoder
    , authHeader
    , createRequest
    , fetchAuth
    , fetchContinueWatching
    , fetchDiscoverPopular
    , fetchDiscoverSearch
    , fetchLibrary
    , fetchItemMetadata
    , fetchNew
    , fetchPlaybackInfo
    , fetchPopular
    , fetchRequests
    , fetchSeriesEpisodes
    , fetchSeriesSearch
    , fetchTrackers
    , imageUrl
    , itemMetadataDecoder
    , searchIndexers
    , searchIndexersTv
    , searchReleases
    , streamUrl
    , transcodingSrc
    )

{-| Client for the functonal-media-server HTTP API. Progress reporting
(POST /progress) is done directly from JS in index.html on a timer, not
through Elm, so there's no Elm-side function for it. -}

import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Url


{-| API base: RELATIVE ("") so every request goes to whatever origin the
page was served from (m69) -- http://localhost:8080 on this machine, but
also http://192.168.1.95:8080 from a laptop on the LAN. -}
baseUrl : String
baseUrl =
    ""


type alias Item =
    { id : String
    , title : String
    , size : Int
    , mime : String
    , displayTitle : String
    , itemType : String
    , seriesName : Maybe String
    , itemSeason : Maybe Int
    , itemEpisode : Maybe Int
    , genres : List String
    }


{-| m40: optional displayTitle (server sends the TMDB title); falls back
to the raw file/release name in `title`. Accepts PascalCase too. -}
displayTitleField : Decoder (Maybe String)
displayTitleField =
    Decode.maybe
        (Decode.oneOf
            [ Decode.field "displayTitle" Decode.string
            , Decode.field "DisplayTitle" Decode.string
            ]
        )


{-| m43: item type ("movie"/"episode"/...) from lowercase `type` or
PascalCase `Type`, defaulting to "movie". -}
itemTypeField : Decoder String
itemTypeField =
    Decode.map (Maybe.withDefault "movie")
        (Decode.maybe
            (Decode.oneOf
                [ Decode.field "type" Decode.string
                , Decode.field "Type" Decode.string
                ]
            )
        )


{-| m43: optional series name for episodes. -}
seriesNameField : Decoder (Maybe String)
seriesNameField =
    Decode.maybe
        (Decode.oneOf
            [ Decode.field "seriesName" Decode.string
            , Decode.field "SeriesName" Decode.string
            ]
        )



{-| m47: season/episode numbers on legacy rows (lowercase) and Jellyfin
items (PascalCase). -}
itemSeasonField : Decoder (Maybe Int)
itemSeasonField =
    Decode.maybe
        (Decode.oneOf
            [ Decode.field "seasonNumber" Decode.int
            , Decode.field "SeasonNumber" Decode.int
            ]
        )


itemEpisodeField : Decoder (Maybe Int)
itemEpisodeField =
    Decode.maybe
        (Decode.oneOf
            [ Decode.field "episodeNumber" Decode.int
            , Decode.field "EpisodeNumber" Decode.int
            ]
        )


{-| m53b: genre list from legacy rows (lowercase `genres`) or Jellyfin
items (PascalCase `Genres`), default []. -}
itemGenresField : Decoder (List String)
itemGenresField =
    Decode.map (Maybe.withDefault [])
        (Decode.maybe
            (Decode.oneOf
                [ Decode.field "genres" (Decode.list Decode.string)
                , Decode.field "Genres" (Decode.list Decode.string)
                ]
            )
        )

{-| 7-field constructor; itemSeason/itemEpisode are layered on by map2
(the pinned elm/json has no map9+). -}
mkItem : String -> String -> Int -> String -> String -> String -> Maybe String -> Item
mkItem id title size mime displayTitle itemType seriesName =
    Item id title size mime displayTitle itemType seriesName Nothing Nothing []


itemDecoder : Decoder Item
itemDecoder =
    Decode.map2 (\i md -> { i | displayTitle = Maybe.withDefault i.title md })
        (Decode.map2 (\i gs -> { i | genres = gs })
            (Decode.map2 (\i se -> { i | itemSeason = se })
                (Decode.map2 (\i ep -> { i | itemEpisode = ep })
                    (Decode.map7 mkItem
                    (Decode.field "id" Decode.string)
                    (Decode.field "title" Decode.string)
                    (Decode.field "size" Decode.int)
                    (Decode.field "mime" Decode.string)
                    (Decode.succeed "")
                        itemTypeField
                        seriesNameField
                    )
                    itemEpisodeField
                )
                itemSeasonField
            )
            itemGenresField
        )
        displayTitleField


{-| A library item with an in-progress watch percentage (0..100), as
returned by `/continuewatching`. -}
type alias ContinueItem =
    { id : String
    , title : String
    , size : Int
    , mime : String
    , progress : Int
    , displayTitle : String
    , itemType : String
    , seriesName : Maybe String
    , itemSeason : Maybe Int
    , itemEpisode : Maybe Int
    }


mkContinueItem : String -> String -> Int -> String -> Int -> String -> String -> Maybe String -> ContinueItem
mkContinueItem id title size mime progress displayTitle itemType seriesName =
    ContinueItem id title size mime progress displayTitle itemType seriesName Nothing Nothing


continueItemDecoder : Decoder ContinueItem
continueItemDecoder =
    Decode.map2 (\i md -> { i | displayTitle = Maybe.withDefault i.title md })
        (Decode.map2 (\i se -> { i | itemSeason = se })
            (Decode.map2 (\i ep -> { i | itemEpisode = ep })
                (Decode.map8 mkContinueItem
                    (Decode.field "id" Decode.string)
                    (Decode.field "title" Decode.string)
                    (Decode.field "size" Decode.int)
                    (Decode.field "mime" Decode.string)
                    (Decode.field "progress" Decode.int)
                    (Decode.succeed "")
                    itemTypeField
                    seriesNameField
                )
                itemEpisodeField
            )
            itemSeasonField
        )
        displayTitleField


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


{-| m34: artwork URL for an item: /Items/:id/Images/<kind>?api_key=<token>
(kind = Primary | Backdrop | ...). Token appended when present (the image
route is auth-gated in strict mode and <img> cannot carry headers). -}
imageUrl : Maybe String -> String -> String -> String
imageUrl token id kind =
    baseUrl
        ++ "/Items/"
        ++ id
        ++ "/Images/"
        ++ kind
        ++ (case token of
                Just t ->
                    "?api_key=" ++ Url.percentEncode t

                Nothing ->
                    ""
           )


{-| m34: the TMDB metadata object stored server-side
(GET /Items/:id/metadata -> {"Metadata": {...}}; lowercase keys).
All fields optional so partial/legacy metadata still decodes. -}
type alias ItemMetadata =
    { title : Maybe String
    , overview : Maybe String
    , year : Maybe Int
    , rating : Maybe Float
    , genres : List String
    }


emptyItemMetadata : ItemMetadata
emptyItemMetadata =
    ItemMetadata Nothing Nothing Nothing Nothing []


itemMetadataDecoder : Decoder ItemMetadata
itemMetadataDecoder =
    Decode.map5 ItemMetadata
        (Decode.maybe (Decode.field "title" Decode.string))
        (Decode.maybe (Decode.field "overview" Decode.string))
        (Decode.maybe (Decode.field "year" Decode.int))
        (Decode.maybe (Decode.field "rating" Decode.float))
        (Decode.field "genres" (Decode.list Decode.string) |> Decode.maybe |> Decode.map (Maybe.withDefault []))


{-| m34: GET /Items/:id/metadata -> the Metadata object (null -> empty). -}
fetchItemMetadata : Maybe String -> String -> (Result Http.Error ItemMetadata -> msg) -> Cmd msg
fetchItemMetadata token itemId toMsg =
    Http.request
        { method = "GET"
        , headers = authHeader token
        , url = baseUrl ++ "/Items/" ++ itemId ++ "/metadata"
        , body = Http.emptyBody
        , expect =
            Http.expectJson toMsg
                (Decode.map (Maybe.withDefault emptyItemMetadata)
                    (Decode.maybe (Decode.field "Metadata" itemMetadataDecoder))
                )
        , timeout = Nothing
        , tracker = Nothing
        }


{-| POST /Items/:id/PlaybackInfo response (m33): PlayMethod
("DirectPlay" | "Transcode") and an optional server-relative
TranscodingUrl. -}
type alias PlaybackInfo =
    { playMethod : String
    , transcodingUrl : Maybe String
    }


playbackInfoDecoder : Decoder PlaybackInfo
playbackInfoDecoder =
    Decode.map2 PlaybackInfo
        (Decode.field "PlayMethod" Decode.string)
        (Decode.maybe (Decode.field "TranscodingUrl" Decode.string))


{-| m33: POST /Items/:id/PlaybackInfo (empty JSON body, auth header) so the
player can pick DirectPlay (/stream/:id) vs Transcode (TranscodingUrl). -}
fetchPlaybackInfo : Maybe String -> String -> (Result Http.Error PlaybackInfo -> msg) -> Cmd msg
fetchPlaybackInfo token itemId toMsg =
    Http.request
        { method = "POST"
        , headers = authHeader token ++ [ Http.header "Content-Type" "application/json" ]
        , url = baseUrl ++ "/Items/" ++ itemId ++ "/PlaybackInfo"
        , body = Http.jsonBody (Encode.object [])
        , expect = Http.expectJson toMsg playbackInfoDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


{-| m33: absolute media URL for a server-relative path (e.g. a
TranscodingUrl), carrying the token as api_key (? or & depending on
whether the path already has a query). -}
transcodingSrc : Maybe String -> String -> String -> String
transcodingSrc token base path =
    base
        ++ path
        ++ (case token of
                Just t ->
                    (if String.contains "?" path then
                        "&api_key="

                     else
                        "?api_key="
                    )
                        ++ Url.percentEncode t

                Nothing ->
                    ""
           )


{-| m32: stream URL with the token as a Jellyfin-style api_key query param,
because a <video src> cannot carry the X-Emby-Token header. `Nothing` (open
mode / not logged in) appends nothing. -}
streamUrl : Maybe String -> String -> String
streamUrl token id =
    baseUrl
        ++ "/stream/"
        ++ id
        ++ (case token of
                Just t ->
                    "?api_key=" ++ Url.percentEncode t

                Nothing ->
                    ""
           )


{-| One media request from GET /Requests (m20a/m20b backend, PascalCase).
m27: downloadDir is the download location stamped at creation (DownloadDir).
m37: progress / torrentState are live Deluge values (Progress / TorrentState). -}
type alias RequestEntry =
    { id : String
    , title : String
    , year : Maybe Int
    , mediaType : String
    , status : String
    , sizeBytes : Maybe Int
    , sourceRelease : Maybe String
    , createdAt : String
    , downloadDir : Maybe String
    , progress : Maybe Float
    , torrentState : Maybe String
    }


requestEntryDecoder : Decoder RequestEntry
requestEntryDecoder =
    -- (map2/map8 only: the pinned elm/json has no map9/required/optional)
    Decode.map2 (\r d -> { r | downloadDir = d })
        (Decode.map2 (\r p -> { r | progress = p })
            (Decode.map2 (\r ts -> { r | torrentState = ts })
                (Decode.map8 (\i t y m s sz sr c -> RequestEntry i t y m s sz sr c Nothing Nothing Nothing)
                    (Decode.field "Id" Decode.string)
                    (Decode.field "Title" Decode.string)
                    (Decode.maybe (Decode.field "Year" Decode.int))
                    (Decode.field "MediaType" Decode.string)
                    (Decode.field "Status" Decode.string)
                    (Decode.maybe (Decode.field "SizeBytes" Decode.int))
                    (Decode.maybe (Decode.field "SourceRelease" Decode.string))
                    (Decode.field "CreatedAt" Decode.string)
                )
                (Decode.maybe (Decode.field "TorrentState" Decode.string))
            )
            (Decode.maybe (Decode.field "Progress" Decode.float))
        )
        (Decode.maybe (Decode.field "DownloadDir" Decode.string))


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


{-| One Torznab release from GET /Indexers/Search (m21/m26, field IndexerId)
or the legacy GET /Trackers/:id/Search (field TrackerId). -}
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
        -- m28: /Indexers/Search results carry IndexerId, legacy tracker
        -- search carries TrackerId -- accept either
        (Decode.oneOf
            [ Decode.field "IndexerId" Decode.string
            , Decode.field "TrackerId" Decode.string
            ]
        )


{-| m28: the /Indexers/Search response is an ENVELOPE
`{"Results": [...], "Errors": {...}}`, not a bare array; the Errors object
is ignored here. -}
searchResultsDecoder : Decoder (List Release)
searchResultsDecoder =
    Decode.field "Results" (Decode.list releaseDecoder)


{-| Body of POST /Requests: Title is required, everything else optional. -}
type alias CreateRequestPayload =
    { title : String
    , year : Maybe Int
    , tmdbId : Maybe Int
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
            ++ encodeMaybe "TmdbId" Encode.int p.tmdbId
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
    , originalName : Maybe String
    , originalLanguage : Maybe String
    , mediaType : String
    , year : Maybe Int
    , overview : Maybe String
    , vote : Maybe Float
    , posterUrl : Maybe String
    }


discoverItemDecoder : Decoder DiscoverItem
discoverItemDecoder =
    Decode.map8
        (\id name originalName originalLanguage mediaType year overview vote ->
            Decode.map (DiscoverItem id name originalName originalLanguage mediaType year overview vote)
                (Decode.maybe (Decode.field "PosterUrl" Decode.string))
        )
        (Decode.field "Id" Decode.string)
        (Decode.field "Name" Decode.string)
        (Decode.maybe (Decode.field "OriginalName" Decode.string))
        (Decode.maybe (Decode.field "OriginalLanguage" Decode.string))
        (Decode.field "Type" Decode.string)
        (Decode.maybe (Decode.field "ProductionYear" Decode.int))
        (Decode.maybe (Decode.field "Overview" Decode.string))
        (Decode.maybe (Decode.field "VoteAverage" Decode.float))
        |> Decode.andThen identity


{-| GET /Discover/Popular -> (movies, shows) from the {"Movies":..,"Shows":..}
envelope. -}
fetchDiscoverPopular : Maybe String -> (Result Http.Error ( List DiscoverItem, List DiscoverItem ) -> msg) -> Cmd msg
fetchDiscoverPopular token toMsg =
    fetchDiscoverSection token "/Discover/Popular" toMsg


{-| m59: GET /Discover/Search?q=.. -> (movies, shows), same envelope as
/Discover/Popular. The Discover tab shows these as TMDB cards; clicking a
card triggers the indexer (torrent) search for that title. -}
fetchDiscoverSearch : Maybe String -> String -> (Result Http.Error ( List DiscoverItem, List DiscoverItem ) -> msg) -> Cmd msg
fetchDiscoverSearch token query toMsg =
    fetchDiscoverSection token ("/Discover/Search?q=" ++ Url.percentEncode query) toMsg


fetchDiscoverSection : Maybe String -> String -> (Result Http.Error ( List DiscoverItem, List DiscoverItem ) -> msg) -> Cmd msg
fetchDiscoverSection token path toMsg =
    Http.request
        { method = "GET"
        , headers = authHeader token
        , url = baseUrl ++ path
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


{-| GET /Indexers/Search?q=..&q=..&type=movie[&indexerId=..] (m26/m28):
with `Nothing` the server searches ALL enabled indexers; with `Just`
only that one. The response is the {Results, Errors} envelope (m28).
m66: `queries` is a LIST -- one repeated @q=@ param per query (the
English display title AND the TMDB original title); the server searches
each and merges the results, so original-language releases (e.g.
Spanish "El laberinto del fauno" for Pan's Labyrinth) show up too. -}
searchIndexers : Maybe String -> Maybe String -> List String -> (Result Http.Error (List Release) -> msg) -> Cmd msg
searchIndexers token mIndexerId queries toMsg =
    searchIndexersUrl token
        (case mIndexerId of
            Just iid ->
                "/Indexers/Search?" ++ qParams queries ++ "&type=movie&indexerId=" ++ Url.percentEncode iid

            Nothing ->
                "/Indexers/Search?" ++ qParams queries ++ "&type=movie"
        )
        toMsg


{-| Repeated @q=<enc>@ params, one per query (m66). -}
qParams : List String -> String
qParams queries =
    String.join "&" (List.map (\q -> "q=" ++ Url.percentEncode q) queries)


{-| m60: TV search for one episode or a season pack from the Discover
tab: @type=tv@ plus torznab @season@/@ep@ params. m66: `queries` is a
list (English series name + original name), each sent as its own @q=. -}
searchIndexersTv : Maybe String -> List String -> Int -> Maybe Int -> (Result Http.Error (List Release) -> msg) -> Cmd msg
searchIndexersTv token queries season mEp toMsg =
    searchIndexersUrl token
        ("/Indexers/Search?" ++ qParams queries
            ++ "&type=tv&season=" ++ String.fromInt season
            ++ (case mEp of
                    Just ep ->
                        "&ep=" ++ String.fromInt ep

                    Nothing ->
                        ""
               )
        )
        toMsg


searchIndexersUrl : Maybe String -> String -> (Result Http.Error (List Release) -> msg) -> Cmd msg
searchIndexersUrl token url toMsg =
    Http.request
        { method = "GET"
        , headers = authHeader token
        , url = baseUrl ++ url
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg searchResultsDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


{-| m47: one hit of GET /Series/Search (the discoverEntry shape with
Type = "Series"). -}
type alias SeriesHit =
    { id : String
    , name : String
    , year : Maybe Int
    , overview : Maybe String
    , posterUrl : Maybe String
    }


seriesHitDecoder : Decoder SeriesHit
seriesHitDecoder =
    Decode.map5 SeriesHit
        (Decode.field "Id" Decode.string)
        (Decode.field "Name" Decode.string)
        (Decode.maybe (Decode.field "ProductionYear" Decode.int))
        (Decode.maybe (Decode.field "Overview" Decode.string))
        (Decode.maybe (Decode.field "PosterUrl" Decode.string))


{-| m47: one episode of GET /Series/:id/Episodes. `downloaded` is set by
the server when a matching library Episode item exists. -}
type alias SeriesEpisode =
    { episodeNumber : Int
    , name : Maybe String
    , overview : Maybe String
    , airDate : Maybe String
    , stillUrl : Maybe String
    , downloaded : Bool
    }


seriesEpisodeDecoder : Decoder SeriesEpisode
seriesEpisodeDecoder =
    Decode.map6 SeriesEpisode
        (Decode.field "EpisodeNumber" Decode.int)
        (Decode.maybe (Decode.field "Name" Decode.string))
        (Decode.maybe (Decode.field "Overview" Decode.string))
        (Decode.maybe (Decode.field "AirDate" Decode.string))
        (Decode.maybe (Decode.field "StillUrl" Decode.string))
        (Decode.field "Downloaded" Decode.bool)


type alias SeriesSeason =
    { seasonNumber : Int
    , name : Maybe String
    , episodes : List SeriesEpisode
    }


seriesSeasonDecoder : Decoder SeriesSeason
seriesSeasonDecoder =
    Decode.map3 SeriesSeason
        (Decode.field "SeasonNumber" Decode.int)
        (Decode.maybe (Decode.field "Name" Decode.string))
        (Decode.field "Episodes" (Decode.list seriesEpisodeDecoder))


{-| m47: the full series view payload of GET /Series/:id/Episodes. -}
type alias SeriesDetail =
    { id : String
    , name : Maybe String
    , year : Maybe Int
    , overview : Maybe String
    , posterUrl : Maybe String
    , seasons : List SeriesSeason
    }


seriesDetailDecoder : Decoder SeriesDetail
seriesDetailDecoder =
    Decode.map6 SeriesDetail
        (Decode.field "Id" Decode.string)
        (Decode.maybe (Decode.field "Name" Decode.string))
        (Decode.maybe (Decode.field "Year" Decode.int))
        (Decode.maybe (Decode.field "Overview" Decode.string))
        (Decode.maybe (Decode.field "PosterUrl" Decode.string))
        (Decode.field "Seasons" (Decode.list seriesSeasonDecoder))


{-| m47: GET /Series/Search?name=.. -> TMDB TV search hits. -}
fetchSeriesSearch : Maybe String -> String -> (Result Http.Error (List SeriesHit) -> msg) -> Cmd msg
fetchSeriesSearch token name toMsg =
    Http.request
        { method = "GET"
        , headers = authHeader token
        , url = baseUrl ++ "/Series/Search?name=" ++ Url.percentEncode name
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg (Decode.list seriesHitDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }


{-| m47: GET /Series/:id/Episodes -> seasons with per-episode Downloaded. -}
fetchSeriesEpisodes : Maybe String -> String -> (Result Http.Error SeriesDetail -> msg) -> Cmd msg
fetchSeriesEpisodes token seriesId toMsg =
    Http.request
        { method = "GET"
        , headers = authHeader token
        , url = baseUrl ++ "/Series/" ++ Url.percentEncode seriesId ++ "/Episodes"
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg seriesDetailDecoder
        , timeout = Nothing
        , tracker = Nothing
        }
