module LiveSearchTests exposing (suite)

import Api
import Expect
import Test exposing (Test, describe, test)
import Types
import Update


discoverQueryModel : String -> Types.Model
discoverQueryModel q =
    let
        ( model, _ ) =
            Types.init ()
    in
    { model
        | screen = Types.Requests
        , requestsTab = Types.Discover
        , reqQuery = q
        , reqLiveSearchAcc = Just 0
        , reqSearchPending = Nothing
    }


updateModel : Types.Msg -> Types.Model -> Types.Model
updateModel msg model =
    Tuple.first (Update.update msg model)


suite : Test
suite =
    describe "Discover live search debounce"
        [ test "typing a nonblank query arms the timer and clears picked state" <|
            \_ ->
                let
                    ( initial, _ ) =
                        Types.init ()

                    picked =
                        { initial
                            | screen = Types.Requests
                            , requestsTab = Types.Discover
                            , reqQuery = "old"
                            , releases = [ fakeRelease ]
                            , discoverShow = Just fakeItem
                            , reqLiveSearchAcc = Just 200
                            , reqSearchPending = Just "old"
                        }

                    afterTyping =
                        updateModel (Types.SetReqQuery "new") picked
                in
                Expect.all
                    [ .reqQuery >> Expect.equal "new"
                    , .reqLiveSearchAcc >> Expect.equal (Just 0)
                    , .reqSearchPending >> Expect.equal Nothing
                    , .releases >> Expect.equal []
                    , .discoverShow >> Expect.equal Nothing
                    ]
                    afterTyping
        , test "blank query disarms the timer" <|
            \_ ->
                updateModel (Types.SetReqQuery "   ") (discoverQueryModel "old")
                    |> .reqLiveSearchAcc
                    |> Expect.equal Nothing
        , test "explicit Search cancels the pending debounce" <|
            \_ ->
                let
                    afterSearch =
                        updateModel Types.SearchReleases (discoverQueryModel "matrix")
                in
                Expect.all
                    [ .reqLiveSearchAcc >> Expect.equal Nothing
                    , .reqSearchPending >> Expect.equal (Just "matrix")
                    ]
                    afterSearch
        , test "ticks accumulate only while a keystroke timer is armed" <|
            \_ ->
                let
                    afterTick =
                        updateModel (Types.Tick 200) (discoverQueryModel "matrix")
                in
                Expect.all
                    [ .reqLiveSearchAcc >> Expect.equal (Just 200)
                    , .reqSearchPending >> Expect.equal Nothing
                    ]
                    afterTick
        , test "the debounce fires at 350ms and records the in-flight query" <|
            \_ ->
                let
                    before =
                        discoverQueryModel "matrix"

                    armed =
                        updateModel (Types.Tick 200) before

                    fired =
                        updateModel (Types.Tick 150) armed
                in
                Expect.all
                    [ .reqLiveSearchAcc >> Expect.equal Nothing
                    , .reqSearchPending >> Expect.equal (Just "matrix")
                    , .reqLoading >> Expect.equal True
                    ]
                    fired
        , test "ticks without an armed timer do not re-arm the search" <|
            \_ ->
                let
                    model =
                        discoverQueryModel "matrix"

                    noTimer =
                        { model | reqLiveSearchAcc = Nothing }

                    afterTicks =
                        updateModel (Types.Tick 1000) noTimer
                in
                Expect.all
                    [ .reqLiveSearchAcc >> Expect.equal Nothing
                    , .reqSearchPending >> Expect.equal Nothing
                    , .reqLoading >> Expect.equal False
                    ]
                    afterTicks
        , test "a late response for an older query is ignored" <|
            \_ ->
                let
                    original =
                        discoverQueryModel "old"

                    current =
                        { original | reqQuery = "new" }
                in
                updateModel
                    (Types.GotDiscoverSearch
                        (Ok ( [ fakeItem ], [] ))
                    )
                    current
                    |> .searchMovies
                    |> Expect.equal []
        , test "a current response clears pending state" <|
            \_ ->
                let
                    model =
                        discoverQueryModel "matrix"
                        |> (\m -> { m | reqSearchPending = Just "matrix", reqLoading = True })
                in
                updateModel (Types.GotDiscoverSearch (Ok ( [ fakeItem ], [] ))) model
                    |> Expect.all
                        [ .searchMovies >> Expect.equal [ fakeItem ]
                        , .reqLoading >> Expect.equal False
                        , .reqSearchPending >> Expect.equal Nothing
                        ]
        , test "switching tabs clears debounce and pending state" <|
            \_ ->
                let
                    model =
                        discoverQueryModel "matrix"
                        |> (\m -> { m | reqSearchPending = Just "matrix" })
                in
                updateModel (Types.SetTab Types.Mine) model
                    |> Expect.all
                        [ .reqLiveSearchAcc >> Expect.equal Nothing
                        , .reqSearchPending >> Expect.equal Nothing
                        ]
        , test "picking a card clears debounce and pending state" <|
            \_ ->
                updateModel (Types.PickShow fakeItem) (discoverQueryModel "matrix")
                    |> Expect.all
                        [ .reqLiveSearchAcc >> Expect.equal Nothing
                        , .reqSearchPending >> Expect.equal Nothing
                        ]
        ]


fakeRelease : Api.Release
fakeRelease =
    { title = "Fake release"
    , sizeBytes = Nothing
    , seeders = Nothing
    , downloadUrl = Nothing
    , magnet = Nothing
    , trackerId = ""
    , indexerName = Nothing
    }


fakeItem : Api.DiscoverItem
fakeItem =
    { id = "1"
    , name = "A title"
    , originalName = Nothing
    , originalLanguage = Nothing
    , mediaType = "movie"
    , year = Nothing
    , overview = Nothing
    , vote = Nothing
    , posterUrl = Nothing
    }
