module Pages.Movies exposing (view)

import Api
import Components exposing (libraryCard)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Types exposing (Model, Msg(..))


{-| m41/m43: real server library items of type "movie" (fallback: video
mime without a series), searched by the global search box.
m53b: genre filter chip row (genres come from the TMDB metadata the
server puts on the legacy rows) + a sort toggle (A-Z / Z-A / newest). -}
view : Model -> Html Msg
view model =
    let
        movies =
            model.library
                |> List.filter isMovie
                |> search model.search
                |> filterGenre model.movieGenreFilter
                |> sortItems model.movieSort

        genres =
            model.library
                |> List.filter isMovie
                |> List.concatMap .genres
                |> unique
                |> List.sort
    in
    div [ class "pa4" ]
        [ div [ class "text-xl ttu tracked-wide mb-3 fg-primary" ] [ text "MOVIES" ]
        , if List.isEmpty genres then
            text ""

          else
            div [ class "flex flex-wrap items-center gap-2 mb-3" ]
                (chip (model.movieGenreFilter == Nothing) "ALL" (SetMovieGenreFilter Nothing)
                    :: List.map (\g -> chip (model.movieGenreFilter == Just g) g (SetMovieGenreFilter (Just g))) genres
                )
        , div [ class "flex items-center justify-end mb-3" ]
            [ div
                [ class "f7 ttu tracked-wide ba b--default fg-secondary pa2 pointer"
                , style "padding" "4px 10px"
                , onClick CycleMovieSort
                ]
                [ text ("SORT: " ++ sortLabel model.movieSort) ]
            ]
        , if List.isEmpty movies then
            div [ class "f6 fg-muted" ] [ text "No movies found." ]

          else
            div [ class "grid-fill" ] (List.map (libraryCard model.token) movies)
        ]


{-| A filter chip; the active one is inverted (filled). -}
chip : Bool -> String -> Msg -> Html Msg
chip active label msg =
    span
        [ class
            (if active then
                "ba b--strong f7 ttu tracked-wide pointer"

             else
                "ba b--default f7 ttu tracked-wide pointer fg-secondary"
            )
        , style "padding" "4px 10px"
        , if active then
            style "background" "var(--white)"

          else
            style "background" "transparent"
        , if active then
            style "color" "var(--black)"

          else
            style "color" ""
        , onClick msg
        ]
        [ text (String.toUpper label) ]


sortLabel : String -> String
sortLabel s =
    case s of
        "za" ->
            "Z-A"

        "new" ->
            "NEWEST"

        _ ->
            "A-Z"


unique : List String -> List String
unique =
    List.foldl (\x acc -> if List.member x acc then acc else acc ++ [ x ]) []


isMovie : Api.Item -> Bool
isMovie i =
    let
        ty =
            String.toLower i.itemType
    in
    ty == "movie"
        || (ty == "" && String.startsWith "video/" i.mime && i.seriesName == Nothing)


filterGenre : Maybe String -> List Api.Item -> List Api.Item
filterGenre mg items =
    case mg of
        Nothing ->
            items

        Just g ->
            let
                lg =
                    String.toLower g
            in
            List.filter (\i -> List.any (\x -> String.toLower x == lg) i.genres) items


sortItems : String -> List Api.Item -> List Api.Item
sortItems mode items =
    case mode of
        "za" ->
            List.sortWith (\a b -> compare (String.toLower b.displayTitle) (String.toLower a.displayTitle)) items

        "new" ->
            -- server ids are sequential: higher id = newer
            List.sortWith (\a b -> compare (idNum b) (idNum a)) items

        _ ->
            List.sortWith (\a b -> compare (String.toLower a.displayTitle) (String.toLower b.displayTitle)) items


idNum : Api.Item -> Int
idNum i =
    String.toInt i.id |> Maybe.withDefault 0


search : String -> List Api.Item -> List Api.Item
search query items =
    if String.isEmpty query then
        items

    else
        let
            lower =
                String.toLower query
        in
        List.filter (\item -> String.contains lower (String.toLower item.displayTitle) || String.contains lower (String.toLower item.title)) items
