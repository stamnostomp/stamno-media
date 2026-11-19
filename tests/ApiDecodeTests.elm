module ApiDecodeTests exposing (suite)

{-| m28: decoder tests for the /Indexers/Search envelope
{Results, Errors} with IndexerId fields, plus the legacy bare
TrackerId shape from /Trackers/:id/Search. -}

import Api
import Debug
import Expect
import Json.Decode as Decode
import Test exposing (Test, describe, test)


envelopeSample : String
envelopeSample =
    """{"Results":[{"Title":"Loop Movie 2020 1080p","SizeBytes":2000000000,"Seeders":9,"IndexerId":"seed","IndexerName":"mock","ReleaseId":"abc","DownloadUrl":"/Indexers/seed/Download?r=abc","Magnet":null},{"Title":"Loop Movie 2020 2160p","SizeBytes":null,"Seeders":null,"IndexerId":"seed","DownloadUrl":null,"Magnet":"magnet:?xt=urn:btih:zz"}],"Errors":{"other":"not logged in (cookie missing/expired)"}}"""


legacyTrackerSample : String
legacyTrackerSample =
    """[{"Title":"Old Release","SizeBytes":123,"Seeders":4,"TrackerId":"jackett","DownloadUrl":"http://j/dl/1.torrent","Magnet":null}]"""


suite : Test
suite =
    describe "Api decoders (m28)"
        [ test "searchResultsDecoder reads the {Results, Errors} envelope" <|
            \_ ->
                case Decode.decodeString Api.searchResultsDecoder envelopeSample of
                    Ok releases ->
                        Expect.all
                            [ \rs -> Expect.equal (List.length rs) 2
                            , \rs -> Expect.equal (List.map .title rs) [ "Loop Movie 2020 1080p", "Loop Movie 2020 2160p" ]
                            , \rs -> Expect.equal (List.map .trackerId rs) [ "seed", "seed" ]
                            , \rs -> Expect.equal (List.map .sizeBytes rs) [ Just 2000000000, Nothing ]
                            , \rs -> Expect.equal (List.map .seeders rs) [ Just 9, Nothing ]
                            , \rs -> Expect.equal (List.map .magnet rs) [ Nothing, Just "magnet:?xt=urn:btih:zz" ]
                            , \rs -> Expect.equal (List.map .downloadUrl rs) [ Just "/Indexers/seed/Download?r=abc", Nothing ]
                            ]
                            releases

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "releaseDecoder falls back to TrackerId (legacy /Trackers/:id/Search)" <|
            \_ ->
                case Decode.decodeString (Decode.list Api.releaseDecoder) legacyTrackerSample of
                    Ok releases ->
                        Expect.all
                            [ \rs -> Expect.equal (List.length rs) 1
                            , \rs -> Expect.equal (List.map .trackerId rs) [ "jackett" ]
                            , \rs -> Expect.equal (List.map .downloadUrl rs) [ Just "http://j/dl/1.torrent" ]
                            ]
                            releases

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "releaseDecoder rejects an item with neither IndexerId nor TrackerId" <|
            \_ ->
                Decode.decodeString Api.releaseDecoder """{"Title":"NoId"}"""
                    |> Result.toMaybe
                    |> Expect.equal Nothing
              , test "m34 itemMetadataDecoder: full metadata object (lowercase keys)" <|
            \_ ->
                case Decode.decodeString Api.itemMetadataDecoder
                    """{"title":"Shrek","overview":"An ogre.","year":2001,"rating":8.1,"genres":["Animation","Comedy"],"providerIds":{"Tmdb":808}}""" of
                    Ok m ->
                        Expect.all
                            [ \x -> Expect.equal x.title (Just "Shrek")
                            , \x -> Expect.equal x.overview (Just "An ogre.")
                            , \x -> Expect.equal x.year (Just 2001)
                            , \x -> Expect.equal x.rating (Just 8.1)
                            , \x -> Expect.equal x.genres [ "Animation", "Comedy" ]
                            ]
                            m

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "m34 itemMetadataDecoder: missing fields tolerated, envelope Metadata null -> Nothing" <|
            \_ ->
                case Decode.decodeString Api.itemMetadataDecoder """{"title":"Only Title"}""" of
                    Ok m ->
                        Expect.all
                            [ \_ -> Expect.equal m.title (Just "Only Title")
                            , \_ -> Expect.equal m.genres []
                            , \_ -> Expect.equal m.rating Nothing
                            ]
                            ()

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "m34 metadata envelope: Metadata null decodes to the empty record (fetch default)" <|
            \_ ->
                case Decode.decodeString
                    (Decode.maybe (Decode.field "Metadata" Api.itemMetadataDecoder))
                    """{"Id":"1","Name":"x","Metadata":null}""" of
                    Ok (Just m) ->
                        Expect.all
                            [ \_ -> Expect.equal m.title Nothing
                            , \_ -> Expect.equal m.genres []
                            ]
                            ()

                    other ->
                        Expect.fail ("unexpected: " ++ Debug.toString other)
        , test "m47 seriesDetailDecoder: full /Series/:id/Episodes payload" <|
            \_ ->
                case Decode.decodeString Api.seriesDetailDecoder
                    """{"Id":"1942","Name":"Family Guy","Year":1999,"Overview":"A cartoon.","PosterUrl":"http://img/fg.jpg","Seasons":[{"SeasonNumber":25,"Name":"Season 25","Episodes":[{"EpisodeNumber":0,"Name":"Happy Hell-o-ween","Overview":"o0","AirDate":"2024-10-27","StillUrl":"http://img/s0.jpg","Downloaded":true},{"EpisodeNumber":1,"Name":"Ep One","Overview":null,"AirDate":null,"StillUrl":null,"Downloaded":false}]},{"SeasonNumber":0,"Name":"Specials","Episodes":[]}]}""" of
                    Ok d ->
                        Expect.all
                            [ \_ -> Expect.equal d.id "1942"
                            , \_ -> Expect.equal d.name (Just "Family Guy")
                            , \_ -> Expect.equal d.year (Just 1999)
                            , \_ -> Expect.equal d.posterUrl (Just "http://img/fg.jpg")
                            , \_ -> Expect.equal (List.map .seasonNumber d.seasons) [ 25, 0 ]
                            , \_ -> Expect.equal (List.map .episodeNumber (List.head d.seasons |> Maybe.map .episodes |> Maybe.withDefault [])) [ 0, 1 ]
                            , \_ -> Expect.equal (List.map .downloaded (List.concatMap .episodes d.seasons)) [ True, False ]
                            , \_ -> Expect.equal (List.map .name (List.concatMap .episodes d.seasons)) [ Just "Happy Hell-o-ween", Just "Ep One" ]
                            ]
                            ()

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "m47 seriesHitDecoder: /Series/Search entry" <|
            \_ ->
                case Decode.decodeString Api.seriesHitDecoder
                    """{"Id":"1942","Name":"Family Guy","Type":"Series","ProductionYear":1999,"Overview":"A cartoon.","VoteAverage":8.9,"PosterUrl":"http://img/fg.jpg"}""" of
                    Ok h ->
                        Expect.all
                            [ \_ -> Expect.equal h.id "1942"
                            , \_ -> Expect.equal h.name "Family Guy"
                            , \_ -> Expect.equal h.year (Just 1999)
                            , \_ -> Expect.equal h.posterUrl (Just "http://img/fg.jpg")
                            ]
                            ()

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "m47 seriesDetailDecoder: empty graceful payload (no key / TMDB down)" <|
            \_ ->
                case Decode.decodeString Api.seriesDetailDecoder
                    """{"Id":"1942","Name":null,"Year":null,"Overview":null,"PosterUrl":null,"Seasons":[]}""" of
                    Ok d ->
                        Expect.all
                            [ \_ -> Expect.equal d.name Nothing
                            , \_ -> Expect.equal d.seasons []
                            ]
                            ()

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        ]
