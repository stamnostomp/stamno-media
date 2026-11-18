module ApiDecodeTests exposing (suite)

{-| m28: decoder tests for the /Indexers/Search envelope
{Results, Errors} with IndexerId fields, plus the legacy bare
TrackerId shape from /Trackers/:id/Search. -}

import Api
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
        ]
