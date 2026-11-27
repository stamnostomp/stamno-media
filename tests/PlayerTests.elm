module PlayerTests exposing (suite)

import Api
import Expect
import Test exposing (Test, describe, test)
import Types
import Update


mkItem : String -> String -> Maybe String -> Maybe Int -> Maybe Int -> Api.Item
mkItem id itemType seriesName season episode =
    { id = id
    , title = id
    , size = 1
    , mime = "video/x-matroska"
    , displayTitle = id
    , itemType = itemType
    , seriesName = seriesName
    , itemSeason = season
    , itemEpisode = episode
    , genres = []
    , path = ""
    }


mkEp : Int -> Bool -> Api.SeriesEpisode
mkEp n downloaded =
    { episodeNumber = n
    , name = Just ("Episode " ++ String.fromInt n)
    , overview = Nothing
    , airDate = Nothing
    , stillUrl = Nothing
    , downloaded = downloaded
    }


mkSeason : Int -> List Api.SeriesEpisode -> Api.SeriesSeason
mkSeason n eps =
    { seasonNumber = n, name = Just ("Season " ++ String.fromInt n), episodes = eps }


mkDetail : List Api.SeriesSeason -> Api.SeriesDetail
mkDetail seasons =
    { id = "1434", name = Just "Family Guy", year = Just 1999, overview = Nothing, posterUrl = Nothing, seasons = seasons }


nowPlayingId : Types.Model -> Maybe String
nowPlayingId model =
    case model.nowPlaying of
        Types.RealPlaying item ->
            Just item.id

        Types.DemoPlaying ->
            Nothing


autoplayModel : Types.Model
autoplayModel =
    let
        ( initial, _ ) =
            Types.init ()

        current =
            mkItem "9" "Episode" (Just "Family Guy") (Just 25) (Just 0)

        next =
            mkItem "10" "Episode" (Just "Family Guy") (Just 25) (Just 1)
    in
    { initial
        | library = [ current, next ]
        , seriesDetail = Just (mkDetail [ mkSeason 25 [ mkEp 0 True, mkEp 1 True ] ])
        , nowPlaying = Types.RealPlaying current
    }


suite : Test
suite =
    describe "Player helpers (m74: skip intro + next episode)"
        [ describe "nextEpisodeItem"
            [ test "next downloaded episode in the same season" <|
                \_ ->
                    let
                        detail =
                            mkDetail [ mkSeason 25 [ mkEp 0 True, mkEp 1 True, mkEp 2 False ] ]

                        library =
                            [ mkItem "9" "Episode" (Just "Family Guy") (Just 25) (Just 0)
                            , mkItem "10" "Episode" (Just "Family Guy") (Just 25) (Just 1)
                            ]

                        cur =
                            mkItem "9" "Episode" (Just "Family Guy") (Just 25) (Just 0)
                    in
                    Types.nextEpisodeItem library (Just detail) cur
                        |> Maybe.map .id
                        |> Expect.equal (Just "10")
            , test "wraps to the next season, skipping specials and non-downloaded" <|
                \_ ->
                    let
                        detail =
                            mkDetail
                                [ mkSeason 24 [ mkEp 20 True ]
                                , mkSeason 25 [ mkEp 0 True, mkEp 1 False, mkEp 2 True ]
                                , mkSeason 0 [ mkEp 5 True ]
                                ]

                        library =
                            [ mkItem "a" "Episode" (Just "Family Guy") (Just 24) (Just 20)
                            , mkItem "b" "Episode" (Just "Family Guy") (Just 25) (Just 0)
                            , mkItem "c" "Episode" (Just "Family Guy") (Just 25) (Just 2)
                            ]

                        cur =
                            mkItem "a" "Episode" (Just "Family Guy") (Just 24) (Just 20)
                    in
                    Types.nextEpisodeItem library (Just detail) cur
                        |> Maybe.map .id
                        |> Expect.equal (Just "b")
            , test "nothing after the last downloaded episode" <|
                \_ ->
                    let
                        detail =
                            mkDetail [ mkSeason 25 [ mkEp 0 True ] ]

                        library =
                            [ mkItem "9" "Episode" (Just "Family Guy") (Just 25) (Just 0) ]

                        cur =
                            mkItem "9" "Episode" (Just "Family Guy") (Just 25) (Just 0)
                    in
                    Types.nextEpisodeItem library (Just detail) cur
                        |> Expect.equal Nothing
            , test "movies have no next episode" <|
                \_ ->
                    Types.nextEpisodeItem [] Nothing (mkItem "4" "Movie" Nothing Nothing Nothing)
                        |> Expect.equal Nothing
            , test "no series detail loaded" <|
                \_ ->
                    Types.nextEpisodeItem [] Nothing (mkItem "9" "Episode" (Just "Family Guy") (Just 25) (Just 0))
                        |> Expect.equal Nothing
            ]
        , describe "video ended event autoplay"
            [ test "matching item id advances to the next episode" <|
                \_ ->
                    Update.update (Types.VideoEnded "9") autoplayModel
                        |> Tuple.first
                        |> nowPlayingId
                        |> Expect.equal (Just "10")
            , test "stale item id is ignored" <|
                \_ ->
                    Update.update (Types.VideoEnded "old") autoplayModel
                        |> Tuple.first
                        |> nowPlayingId
                        |> Expect.equal (Just "9")
            , test "repeat ended events for the same playback are ignored" <|
                \_ ->
                    let
                        handled =
                            { autoplayModel | endedHandled = True }
                    in
                    Update.update (Types.VideoEnded "9") handled
                        |> Tuple.first
                        |> nowPlayingId
                        |> Expect.equal (Just "9")
            , test "opening a real player resets the ended guard" <|
                \_ ->
                    let
                        handled =
                            { autoplayModel | endedHandled = True }

                        item =
                            mkItem "9" "Episode" (Just "Family Guy") (Just 25) (Just 0)
                    in
                    Update.update (Types.OpenRealPlayer item) handled
                        |> Tuple.first
                        |> .endedHandled
                        |> Expect.equal False
            ]
        ]
