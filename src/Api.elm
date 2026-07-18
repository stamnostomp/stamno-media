module Api exposing
    ( ContinueItem
    , Item
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


fetchLibrary : (Result Http.Error (List Item) -> msg) -> Cmd msg
fetchLibrary toMsg =
    Http.get
        { url = baseUrl ++ "/library"
        , expect = Http.expectJson toMsg (Decode.list itemDecoder)
        }


fetchContinueWatching : (Result Http.Error (List ContinueItem) -> msg) -> Cmd msg
fetchContinueWatching toMsg =
    Http.get
        { url = baseUrl ++ "/continuewatching"
        , expect = Http.expectJson toMsg (Decode.list continueItemDecoder)
        }


fetchPopular : (Result Http.Error (List Item) -> msg) -> Cmd msg
fetchPopular toMsg =
    Http.get
        { url = baseUrl ++ "/popular"
        , expect = Http.expectJson toMsg (Decode.list itemDecoder)
        }


fetchNew : (Result Http.Error (List Item) -> msg) -> Cmd msg
fetchNew toMsg =
    Http.get
        { url = baseUrl ++ "/new"
        , expect = Http.expectJson toMsg (Decode.list itemDecoder)
        }


streamUrl : String -> String
streamUrl id =
    baseUrl ++ "/stream/" ++ id
