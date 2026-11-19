module Pages.Series exposing (view)

{-| m47: series detail view. Opened from the Shows page by clicking a
series header: the TMDB id is resolved via /Series/Search, then
/Series/:id/Episodes lists every season/episode. Episodes present in the
library (server-side Downloaded flag) get a PLAY button; the rest are
greyed out and not playable. -}

import Api
import Html exposing (Html, button, div, img, span, text)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (onClick)
import Types exposing (Model, Msg(..), Screen(..))


view : Model -> Html Msg
view model =
    case model.screen of
        SeriesView name ->
            let
                detail =
                    model.seriesDetail

                title =
                    detail
                        |> Maybe.andThen .name
                        |> Maybe.withDefault name

                metaLine =
                    [ detail |> Maybe.andThen .year |> Maybe.map String.fromInt |> Maybe.withDefault "" ]
                        |> List.filter (not << String.isEmpty)
                        |> String.join "  "

                seasons =
                    detail
                        |> Maybe.map .seasons
                        |> Maybe.withDefault []
            in
            div [ class "pa4" ]
                [ div [ class "flex items-start gap-5 mb-5" ]
                    [ case detail |> Maybe.andThen .posterUrl of
                        Just url ->
                            img
                                [ src url
                                , class "flex-none"
                                , style "width" "160px"
                                , style "height" "240px"
                                , style "object-fit" "cover"
                                ]
                                []

                        Nothing ->
                            div
                                [ class "flex-none flex items-center justify-center fg-ghost b ba b--default"
                                , style "width" "160px"
                                , style "height" "240px"
                                , style "font-size" "64px"
                                ]
                                [ text (String.left 1 title) ]
                    , div [ class "flex flex-column gap-3" ]
                        [ div [ class "text-xl ttu tracked-wide fg-primary" ] [ text (String.toUpper title) ]
                        , if String.isEmpty metaLine then
                            text ""

                          else
                            div [ class "f6 fg-secondary ttu tracked-wider" ] [ text metaLine ]
                        , div [ class "f7 fg-muted", style "max-width" "640px" ]
                            [ text (detail |> Maybe.andThen .overview |> Maybe.withDefault "") ]
                        ]
                    ]
                , if List.isEmpty seasons then
                    div [ class "f6 fg-muted" ] [ text "NO EPISODE DATA" ]

                  else
                    div [ class "flex flex-column gap-5" ]
                        (List.map (seasonSection model title) seasons)
                ]

        _ ->
            text ""


seasonSection : Model -> String -> Api.SeriesSeason -> Html Msg
seasonSection model seriesName season =
    let
        heading =
            case season.name of
                Just n ->
                    n

                Nothing ->
                    "SEASON " ++ String.fromInt season.seasonNumber
    in
    div []
        [ div [ class "f6 tracked-wider ttu fg-secondary mb3" ] [ text heading ]
        , div [ class "flex flex-column gap-2" ]
            (List.map (episodeRow model seriesName season.seasonNumber) season.episodes)
        ]


episodeRow : Model -> String -> Int -> Api.SeriesEpisode -> Html Msg
episodeRow model seriesName season ep =
    let
        label =
            "S" ++ pad2 season ++ "E" ++ pad2 ep.episodeNumber

        mItem =
            if ep.downloaded then
                findLibraryItem model.library seriesName season ep.episodeNumber

            else
                Nothing

        title =
            label ++ "  " ++ Maybe.withDefault "" ep.name
    in
    div
        [ class "flex items-center gap-4 pa3 ba b--default bg-panel"
        , style "opacity" (if ep.downloaded then "1" else "0.45")
        ]
        [ case ep.stillUrl of
            Just url ->
                img
                    [ src url
                    , class "flex-none"
                    , style "width" "120px"
                    , style "height" "68px"
                    , style "object-fit" "cover"
                    ]
                    []

            Nothing ->
                div
                    [ class "flex-none flex items-center justify-center fg-ghost b ba b--default"
                    , style "width" "120px"
                    , style "height" "68px"
                    ]
                    [ text (String.left 1 title) ]
        , div [ class "flex flex-column gap-1" ]
            [ div [ class "f6 ttu tracked-wide fg-primary" ] [ text title ]
            , div [ class "f7 fg-muted" ] [ text (Maybe.withDefault "" ep.overview) ]
            , div [ class "f7 fg-ghost" ] [ text (Maybe.withDefault "" ep.airDate) ]
            ]
        , div [ class "ml-auto" ]
            [ if ep.downloaded then
                case mItem of
                    Just item ->
                        button
                            [ class "btn btn-primary f7 ttu tracked-wide"
                            , onClick (OpenRealPlayer item)
                            ]
                            [ text "PLAY" ]

                    Nothing ->
                        span [ class "f7 ttu tracked-wide fg-muted" ] [ text "DOWNLOADED" ]

              else
                span [ class "f7 ttu tracked-wide fg-ghost" ] [ text "NOT DOWNLOADED" ]
            ]
        ]


pad2 : Int -> String
pad2 n =
    String.padLeft 2 '0' (String.fromInt n)


{-| The library Episode item matching series (case-insensitive) + S/E --
the same matching the server uses for the Downloaded flag. -}
findLibraryItem : List Api.Item -> String -> Int -> Int -> Maybe Api.Item
findLibraryItem library seriesName season ep =
    List.head
        (List.filter
            (\i ->
                String.toLower i.itemType == "episode"
                    && String.toLower (Maybe.withDefault "" i.seriesName) == String.toLower seriesName
                    && i.itemSeason == Just season
                    && i.itemEpisode == Just ep
            )
            library
        )
