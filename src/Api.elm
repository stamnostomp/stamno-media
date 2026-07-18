module Api exposing (Item, fetchLibrary, streamUrl)

{-| Client for the functonal-media-server HTTP API. -}

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


fetchLibrary : (Result Http.Error (List Item) -> msg) -> Cmd msg
fetchLibrary toMsg =
    Http.get
        { url = baseUrl ++ "/library"
        , expect = Http.expectJson toMsg (Decode.list itemDecoder)
        }


streamUrl : String -> String
streamUrl id =
    baseUrl ++ "/stream/" ++ id
