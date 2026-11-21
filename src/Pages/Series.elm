module Pages.Series exposing
    ( findLibraryItem
    , playTargetEpisode
    , seriesEpisodesView
    , seriesHeaderView
    , view
    )

{-| m47: series detail view. Opened from the Shows page by clicking a
series header: the TMDB id is resolved via /Series/Search, then
/Series/:id/Episodes lists every season/episode. Episodes present in the
library (server-side Downloaded flag) get a PLAY button; the rest are
greyed out and not playable.
m55: the header + season-select/episode-card views are exported as shared
helpers ('seriesHeaderView' / 'seriesEpisodesView') so the card detail
page (RealDetail) can render the same series section for episode cards. -}

import Api
import Html exposing (Html, button, div, img, option, select, span, text)
import Html.Attributes exposing (class, selected, src, style, value)
import Html.Events exposing (onClick, onInput)
import Types exposing (Model, Msg(..), Screen(..))


view : Model -> Html Msg
view model =
    case model.screen of
        SeriesView name ->
            let
                title =
                    model.seriesDetail
                        |> Maybe.andThen .name
                        |> Maybe.withDefault name
            in
            div [ class "pa4" ]
                [ seriesHeaderView model title
                , seriesEpisodesView model title
                ]

        _ ->
            text ""


{-| m55 (shared): the series header -- poster (TMDB posterUrl when present,
else a letter placeholder), title, year, overview. -}
seriesHeaderView : Model -> String -> Html Msg
seriesHeaderView model title =
    let
        detail =
            model.seriesDetail

        metaLine =
            [ detail |> Maybe.andThen .year |> Maybe.map String.fromInt |> Maybe.withDefault "" ]
                |> List.filter (not << String.isEmpty)
                |> String.join "  "
    in
    div [ class "flex items-start gap-5 mb-5" ]
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


{-| m54/m55 (shared): the season select box + the selected season's episode
cards (downloaded -> PLAY, others greyed NOT DOWNLOADED). -}
seriesEpisodesView : Model -> String -> Html Msg
seriesEpisodesView model title =
    let
        seasons =
            model.seriesDetail
                |> Maybe.map .seasons
                |> Maybe.withDefault []

        -- m54: the season shown in the card grid: the selected one,
        -- else the first
        selectedSeason =
            case model.seriesSeason of
                Just n ->
                    List.head (List.filter (\sn -> sn.seasonNumber == n) seasons)

                Nothing ->
                    List.head seasons
    in
    if List.isEmpty seasons then
        div [ class "f6 fg-muted" ] [ text "NO EPISODE DATA" ]

    else
        div [ class "flex flex-column gap-4" ]
            [ select
                [ class "f6 tracked-wide bg-panel ba b--default fg-primary mb-4"
                , style "padding" "8px 12px"
                , onInput (\str -> case String.toInt str of
                    Just n ->
                        SetSeriesSeason n

                    Nothing ->
                        NoOp)
                ]
                (List.map (seasonOption selectedSeason) seasons)
            , case selectedSeason of
                Just sn ->
                    seasonSection model title sn

                Nothing ->
                    text ""
            ]


{-| m55 (shared, pure): the PLAY target for the series of the active detail
item -- the episode with continue-watching progress (0 < p < 100) if any,
else the first downloaded episode's library item, else Nothing. -}
playTargetEpisode : Model -> Maybe Api.Item
playTargetEpisode model =
    case model.activeRealItem |> Maybe.andThen .seriesName of
        Nothing ->
            Nothing

        Just seriesName ->
            let
                seasons =
                    model.seriesDetail |> Maybe.map .seasons |> Maybe.withDefault []

                downloaded =
                    List.concatMap
                        (\s -> List.map (\e -> ( s.seasonNumber, e.episodeNumber )) (List.filter .downloaded s.episodes))
                        seasons

                resumeCw =
                    model.continueWatching
                        |> List.filter
                            (\c ->
                                String.toLower (Maybe.withDefault "" c.seriesName) == String.toLower seriesName
                                    && c.progress > 0
                                    && c.progress < 100
                            )
                        |> List.head
                        |> Maybe.andThen (\c -> List.head (List.filter (\i -> i.id == c.id) model.library))
            in
            case resumeCw of
                Just item ->
                    Just item

                Nothing ->
                    List.head downloaded
                        |> Maybe.andThen (\( se, ep ) -> findLibraryItem model.library seriesName se ep)


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
findLibraryItem =
    Types.findLibraryItem


{-| m54: one <option> per season; season 0 is labelled SPECIALS. -}
seasonOption : Maybe Api.SeriesSeason -> Api.SeriesSeason -> Html Msg
seasonOption sel sn =
    let
        label =
            if sn.seasonNumber == 0 then
                "SPECIALS"

            else
                "SEASON " ++ String.fromInt sn.seasonNumber
    in
    option
        [ value (String.fromInt sn.seasonNumber)
        , selected (sel == Just sn)
        ]
        [ text label ]
