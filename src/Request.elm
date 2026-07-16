module Request exposing
    ( Entry
    , Status(..)
    , discoverable
    , initialEntries
    , request
    , requested
    , statusLabel
    )

{-| Jellyseerr-style request state: which titles are requested and
where they stand. Stand-in for a request-management API.
-}

import Dict exposing (Dict)


type Status
    = Available
    | Processing
    | Pending


type alias Entry =
    { status : Status
    , date : String
    }


statusLabel : Status -> String
statusLabel status =
    case status of
        Available ->
            "AVAILABLE"

        Processing ->
            "PROCESSING"

        Pending ->
            "PENDING"


initialEntries : Dict Int Entry
initialEntries =
    Dict.fromList
        [ ( 2, Entry Available "JUL 02" )
        , ( 6, Entry Available "JUN 18" )
        , ( 9, Entry Processing "JUL 10" )
        , ( 14, Entry Pending "JUL 14" )
        , ( 16, Entry Pending "JUL 12" )
        ]


{-| Optimistic local request: immediately pending. -}
request : Int -> Dict Int Entry -> Dict Int Entry
request id entries =
    Dict.insert id (Entry Pending "JUST NOW") entries


requested : Dict Int Entry -> List { id : Int, entry : Entry }
requested entries =
    Dict.toList entries
        |> List.map (\( id, entry ) -> { id = id, entry = entry })


{-| Titles (by id) not yet requested, for the Discover tab. -}
discoverable : Dict Int Entry -> List { a | id : Int } -> List { a | id : Int }
discoverable entries =
    List.filter (\t -> not (Dict.member t.id entries))
