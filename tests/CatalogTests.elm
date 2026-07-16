module CatalogTests exposing (suite)

import Catalog exposing (Genre(..), MediaType(..))
import Dict
import Expect
import Request
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Stamno Media logic"
        [ describe "Catalog.byGenre"
            [ test "ALL (Nothing) keeps the full catalog" <|
                \_ ->
                    Catalog.byGenre Nothing Catalog.catalog
                        |> List.length
                        |> Expect.equal (List.length Catalog.catalog)
            , test "a genre filter keeps only that genre" <|
                \_ ->
                    Catalog.byGenre (Just Scifi) Catalog.catalog
                        |> List.all (\t -> t.genre == Scifi)
                        |> Expect.equal True
            , test "genre filter is non-empty for every chip" <|
                \_ ->
                    [ Action, Scifi, Drama, Thriller ]
                        |> List.all
                            (\g -> not (List.isEmpty (Catalog.byGenre (Just g) Catalog.catalog)))
                        |> Expect.equal True
            ]
        , describe "Catalog movies/shows split"
            [ test "movies contains only movies" <|
                \_ ->
                    Catalog.movies Catalog.catalog
                        |> List.all (\t -> t.mediaType == Movie)
                        |> Expect.equal True
            , test "shows contains only shows" <|
                \_ ->
                    Catalog.shows Catalog.catalog
                        |> List.all (\t -> t.mediaType == Show)
                        |> Expect.equal True
            , test "split covers the whole catalog" <|
                \_ ->
                    (List.length (Catalog.movies Catalog.catalog)
                        + List.length (Catalog.shows Catalog.catalog)
                    )
                        |> Expect.equal (List.length Catalog.catalog)
            ]
        , describe "Catalog.related"
            [ test "excludes the active title" <|
                \_ ->
                    Catalog.related 1
                        |> List.all (\t -> t.id /= 1)
                        |> Expect.equal True
            , test "returns at most six titles" <|
                \_ ->
                    Catalog.related 1
                        |> List.length
                        |> Expect.atMost 6
            ]
        , describe "Catalog.find"
            [ test "finds a title by id" <|
                \_ ->
                    (Catalog.find 2).title
                        |> Expect.equal "NIGHT DRIVE"
            , test "falls back to the first title for unknown ids" <|
                \_ ->
                    (Catalog.find 999).id
                        |> Expect.equal 1
            ]
        , describe "Catalog.initial"
            [ test "takes the first letter of the title" <|
                \_ ->
                    Catalog.initial (Catalog.find 2)
                        |> Expect.equal "N"
            ]
        , describe "Request state"
            [ test "discover excludes already-requested titles" <|
                \_ ->
                    Request.discoverable Request.initialEntries Catalog.catalog
                        |> List.all (\t -> not (Dict.member t.id Request.initialEntries))
                        |> Expect.equal True
            , test "requesting a title makes it pending JUST NOW" <|
                \_ ->
                    Request.request 3 Request.initialEntries
                        |> Dict.get 3
                        |> Expect.equal (Just { status = Request.Pending, date = "JUST NOW" })
            , test "requesting a title removes it from discover" <|
                \_ ->
                    Request.discoverable (Request.request 3 Request.initialEntries) Catalog.catalog
                        |> List.all (\t -> t.id /= 3)
                        |> Expect.equal True
            , test "status labels match the design" <|
                \_ ->
                    [ Request.Available, Request.Processing, Request.Pending ]
                        |> List.map Request.statusLabel
                        |> Expect.equal [ "AVAILABLE", "PROCESSING", "PENDING" ]
            ]
        ]
