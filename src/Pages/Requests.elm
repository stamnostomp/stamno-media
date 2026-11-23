module Pages.Requests exposing (view)

{-| The Requests page (m20d), wired to the real m20a/m20b server API:
GET /Requests for the Mine tab, GET /Trackers + GET /Trackers/:id/Search
for the Discover tab, POST /Requests to create a request from a release. -}

import Api
import Components exposing (formatBytes)
import Html exposing (Html, button, div, img, input, option, select, span, text)
import Html.Attributes exposing (class, disabled, placeholder, selected, src, style, value)
import Html.Events exposing (onClick, onInput)
import Pages.Series
import Types exposing (Model, Msg(..), Tab(..))


view : Model -> Html Msg
view model =
    div [ class "pa4" ]
        [ div [ class "flex items-center justify-between mb4" ]
            [ div [ class "text-xl ttu tracked-wide fg-primary" ] [ text "Requests" ]
            , button [ class "btn btn-primary", onClick (SetTab Discover) ] [ text "+ New Request" ]
            ]
        , viewNotice model.reqNotice
        , div [ class "flex gap-5 bb b--default mb4" ]
            [ tab model.requestsTab "My Requests" Mine
            , tab model.requestsTab "Discover" Discover
            ]
        , case model.requestsTab of
            Mine ->
                viewMine model

            Discover ->
                viewDiscover model
        ]


{-| Transient banner (request created / duplicate / failure), dismissable. -}
viewNotice : Maybe String -> Html Msg
viewNotice mNotice =
    case mNotice of
        Nothing ->
            text ""

        Just notice ->
            div [ class "ba b--default bg-panel pa2 mb4 f7 ttu tracked-wide fg-muted flex items-center justify-between" ]
                [ text notice
                , div [ class "pointer fg-secondary pl4", onClick ClearReqNotice ] [ text "X" ]
                ]


tab : Tab -> String -> Tab -> Html Msg
tab current label target =
    div
        [ class "pointer pb-3 f6 tracked-wide ttu"
        , class
            (if current == target then
                "fg-primary"

             else
                "fg-secondary"
            )
        , style "border-bottom"
            (if current == target then
                "2px solid var(--white)"

             else
                "2px solid transparent"
            )
        , onClick (SetTab target)
        ]
        [ text label ]


{-| Mine tab: the request queue returned by GET /Requests. -}
viewMine : Model -> Html Msg
viewMine model =
    if List.isEmpty model.myRequests then
        div [ class "f7 fg-muted ttu tracked-wide pa3 bt b--default" ] [ text "NO REQUESTS YET" ]

    else
        div [ class "flex flex-column bt b--default" ]
            (List.map viewRequestRow model.myRequests)


viewRequestRow : Api.RequestEntry -> Html Msg
viewRequestRow r =
    div [ class "req-row flex items-center gap-4 pv3 ph2 bb b--default" ]
        [ div
            [ class "flex-none ba b--default bg-panel flex items-center justify-center text-md fg-ghost b"
            , style "width" "48px"
            , style "height" "48px"
            ]
            [ text (String.left 1 (String.toUpper r.title)) ]
        , div [ class "flex-auto flex flex-column gap-1" ]
            ([ div [ class "f5 ttu tracked-wide" ] [ text (titleWithYear r) ]
             , div [ class "f7 fg-muted" ]
                [ text
                    (String.join " · "
                        (List.filter (not << String.isEmpty)
                            [ r.mediaType
                            , String.slice 0 10 r.createdAt
                            , sizeText r.sizeBytes
                            , Maybe.withDefault "" r.sourceRelease
                            , Maybe.withDefault "" r.downloadDir
                            ]
                        )
                    )
                ]
             ]
                ++ progressElems r
            )
        , statusBadge r.status
        ]


{-| m37: live Deluge progress bar (server attaches Progress/TorrentState
via core.get_torrents_status). m50: the bar renders whenever progress data
exists OR the request is still Downloading -- so an Imported-but-seeding
request keeps its bar (SEEDING 100%) next to the IMPORTED badge. -}
progressElems : Api.RequestEntry -> List (Html Msg)
progressElems r =
    case r.progress of
        Just p ->
            [ progressBar (clamp 0 100 p) (progressLabel r (Just (clamp 0 100 p))) ]

        Nothing ->
            if r.status == "Downloading" then
                [ progressBar 0 (progressLabel r Nothing) ]

            else
                []


progressLabel : Api.RequestEntry -> Maybe Float -> String
progressLabel r mp =
    let
        pct =
            Maybe.withDefault 0 mp
    in
    -- m41: at 100% / Seeding the bar reads SEEDING, not DOWNLOADING
    if pct >= 99.9 || r.torrentState == Just "Seeding" then
        "SEEDING 100%"

    else
        "DOWNLOADING " ++ String.fromInt (round pct) ++ "%"


progressBar : Float -> String -> Html Msg
progressBar pct label =
    div [ class "flex items-center gap-2" ]
        [ div
            [ class "ba b--default flex-none"
            , style "width" "120px"
            , style "height" "6px"
            ]
            [ div
                [ class "bg-white h-100"
                , style "width" (String.fromFloat pct ++ "%")
                ]
                []
            ]
        , div [ class "f7 fg-muted ttu tracked-wide" ] [ text label ]
        ]


titleWithYear : Api.RequestEntry -> String
titleWithYear r =
    case r.year of
        Just y ->
            r.title ++ " (" ++ String.fromInt y ++ ")"

        Nothing ->
            r.title


sizeText : Maybe Int -> String
sizeText mSize =
    case mSize of
        Just s ->
            formatBytes s

        Nothing ->
            ""


{-| Monochrome status badges: distinguished by border style + text tone only. -}
statusBadge : String -> Html Msg
statusBadge status =
    let
        tone =
            case status of
                "Pending" ->
                    "b--default b--dashed fg-muted"

                "Approved" ->
                    "b--default fg-secondary"

                "Downloading" ->
                    "b--default fg-secondary"

                "Imported" ->
                    "b--strong fg-primary"

                "Rejected" ->
                    "b--default fg-muted"

                "Failed" ->
                    "b--default fg-muted"

                _ ->
                    "b--default fg-secondary"
    in
    div
        [ class "ba f7 tracked-wide ttu"
        , class tone
        , style "padding" "4px 12px"
        ]
        [ text (String.toUpper status) ]


{-| Discover tab (m26, m59: no tracker dropdown - searches always go to
ALL enabled indexers server-side): with an empty query show the TMDB
popular movies / shows poster grids; with a query, search and list the
releases as rows. -}
viewDiscover : Model -> Html Msg
viewDiscover model =
    div []
        [ div [ class "flex items-center gap-3 mb4" ]
            [ input
                [ class "f6 tracked-wide"
                , style "min-width" "320px"
                , style "padding" "8px 12px"
                , placeholder "SEARCH$"
                , value model.reqQuery
                , onInput SetReqQuery
                ]
                []
            , button [ class "btn btn-primary", onClick SearchReleases, disabled model.reqLoading ]
                [ text "Search" ]
            ]
        , if model.reqLoading then
            div [ class "f7 fg-muted ttu tracked-wide pa3" ] [ text "SEARCHING..." ]

          else if not (List.isEmpty model.releases) then
            -- m59/m60: a picked card (or episode/season DOWNLOAD) -> torrent list
            div []
                [ if model.discoverShow /= Nothing then
                    div [ class "mb3" ]
                        [ div
                            [ class "pointer f6 fg-secondary tracked-wide"
                            , onClick BackToDiscoverEpisodes
                            ]
                            [ text "< BACK TO EPISODES" ]
                        ]

                  else
                    text ""
                , viewReleaseList model
                ]

          else if model.discoverShow /= Nothing then
            -- m60: a SHOW card -> series picker (seasons/episodes + DOWNLOAD)
            viewShowPicker model

          else if String.isEmpty model.reqQuery then
            viewPopularSections model

          else
            -- m59: the query itself -> TMDB result cards
            viewSearchSections model
        ]


{-| m59: search results as a list - one row per torrent, much better for
scanning many results than cards. The server sorts by seeders desc, so
the first row is the [RECOMMENDED] pick. -}
viewReleaseList : Model -> Html Msg
viewReleaseList model =
    div [ class "flex flex-column bt b--default" ]
        (List.indexedMap (releaseRow model) model.releases)


releaseRow : Model -> Int -> Api.Release -> Html Msg
releaseRow model idx rel =
    div [ class "req-row flex items-center gap-4 pv3 ph2 bb b--default" ]
        [ div [ class "flex-auto flex flex-column gap-1" ]
            [ div [ class "flex items-center gap-2" ]
                [ div [ class "f5 ttu tracked-wide" ] [ text rel.title ]
                , if idx == 0 then
                    recommendedTag

                  else
                    text ""
                ]
            , div [ class "f7 fg-muted" ]
                [ text
                    (String.join " · "
                        (List.filter (not << String.isEmpty)
                            [ sizeText rel.sizeBytes
                            , seedersText rel.seeders
                            -- m90: prefer the IndexerName sent with the release;
                            -- only fall back to the trackers list / UUID
                            , case rel.indexerName of
                                Just n -> n
                                Nothing -> trackerName model rel.trackerId
                            ]
                        )
                    )
                ]
            ]
        , div
            [ class "req-btn tc f7 tracked-wide ttu pointer flex-none"
            , style "padding" "6px 16px"
            , onClick (CreateRelease rel)
            ]
            [ text "+ REQUEST" ]
        ]


{-| m59: [RECOMMENDED] tag on the top-seeders result. -}
recommendedTag : Html Msg
recommendedTag =
    span
        [ class "ba b--strong f7 tracked-wide ttu fg-primary"
        , style "padding" "2px 8px"
        ]
        [ text "[RECOMMENDED]" ]


trackerName : Model -> String -> String
trackerName model tid =
    model.trackers
        |> List.filter (\t -> t.id == tid)
        |> List.head
        |> Maybe.map .name
        |> Maybe.withDefault tid


{-| m26: with an empty query, show the TMDB popular picks as poster grids;
clicking a card searches for that title. -}
viewPopularSections : Model -> Html Msg
viewPopularSections model =
    div []
        [ viewPopularSection "Popular Movies" model.popularMovies
        , viewPopularSection "Popular Shows" model.popularShows
        ]


{-| m59: with a query (and no torrent list yet), show the TMDB search
results as the same poster cards; clicking one runs the indexer search
for that title and swaps in the torrent list. -}
viewSearchSections : Model -> Html Msg
viewSearchSections model =
    if List.isEmpty model.searchMovies && List.isEmpty model.searchShows then
        div [ class "f7 fg-muted ttu tracked-wide pa3" ] [ text "NO RESULTS" ]

    else
        div []
            [ viewPopularSection "Movies" model.searchMovies
            , viewPopularSection "Shows" model.searchShows
            ]


viewPopularSection : String -> List Api.DiscoverItem -> Html Msg
viewPopularSection label items =
    if List.isEmpty items then
        text ""

    else
        div [ class "mb4" ]
            [ div [ class "f7 ttu tracked-wide fg-muted mb2" ] [ text label ]
            , div [ class "grid-fill" ]
                (List.map popularCard items)
            ]


popularCard : Api.DiscoverItem -> Html Msg
popularCard item =
    div
        [ class "relative ba b--default bg-panel overflow-hidden pointer"
        , style "height" "240px"
        , onClick
            (if item.mediaType == "Series" then
                -- m60: shows open the episode picker, movies go straight
                -- to the torrent list
                PickShow item

             else
                PickPopular item
            )
        ]
        [ case item.posterUrl of
            Just url ->
                img
                    [ class "absolute absolute--fill w-100 h-100"
                    , style "object-fit" "cover"
                    , src url
                    ]
                    []

            Nothing ->
                div
                    [ class "absolute absolute--fill flex items-center justify-center fg-ghost b"
                    , style "font-size" "64px"
                    ]
                    [ text (String.left 1 (String.toUpper item.name)) ]
        , div
            [ class "absolute f7 fg-muted"
            , style "top" "8px"
            , style "right" "8px"
            ]
            [ text (voteText item.vote) ]
        , div [ class "absolute left-0 right-0 bottom-0 pa2 flex flex-column gap-2 fade-up-strong" ]
            [ div [ class "f7 ttu tracked-wide", style "min-height" "2.4em" ] [ text (discoverTitleWithYear item) ]
            , div [ class "f7 fg-muted" ] [ text item.mediaType ]
            ]
        ]


discoverTitleWithYear : Api.DiscoverItem -> String
discoverTitleWithYear item =
    case item.year of
        Just y ->
            item.name ++ " (" ++ String.fromInt y ++ ")"

        Nothing ->
            item.name


voteText : Maybe Float -> String
voteText mVote =
    case mVote of
        Just v ->
            if v <= 0 then
                ""  -- m89: unrated titles should not show "RATING 0"

            else
                "RATING " ++ String.fromFloat (toFloat (round (v * 10)) / 10)

        Nothing ->
            ""


seedersText : Maybe Int -> String
seedersText mSeeders =
    case mSeeders of
        Just n ->
            "seeders " ++ String.fromInt n

        Nothing ->
            ""


{-| m60: show picker inside the Discover tab -- same look as the library
series page (header + season box), but every episode row has a DOWNLOAD
button on the right (greyed DOWNLOADED when already in the library), plus
a SEASON PACK button that searches for the whole season. -}
viewShowPicker : Model -> Html Msg
viewShowPicker model =
    let
        title =
            model.discoverShow
                |> Maybe.map .name
                |> Maybe.withDefault ""

        seasons =
            model.seriesDetail
                |> Maybe.map .seasons
                |> Maybe.withDefault []

        selectedSeason =
            case model.seriesSeason of
                Just n ->
                    List.head (List.filter (\sn -> sn.seasonNumber == n) seasons)

                Nothing ->
                    List.head seasons
    in
    div []
        [ div [ class "flex items-center gap-4 mb4" ]
            [ div
                [ class "pointer f6 fg-secondary tracked-wide ttu"
                , onClick BackToDiscoverCards
                ]
                [ text "< BACK" ]
            , div [ class "text-xl ttu tracked-wide fg-primary" ] [ text (String.toUpper title) ]
            ]
        , Pages.Series.seriesHeaderView model title
        , if List.isEmpty seasons then
            div [ class "f6 fg-muted" ] [ text "NO EPISODE DATA" ]

          else
            div [ class "flex flex-column gap-4" ]
                [ select
                    [ class "f6 tracked-wide bg-panel ba b--default fg-primary mb-4"
                    , style "padding" "8px 12px"
                    , onInput
                        (\str ->
                            case String.toInt str of
                                Just n ->
                                    SetSeriesSeason n

                                Nothing ->
                                    NoOp
                        )
                    ]
                    (List.map (seasonOption selectedSeason) seasons)
                , case selectedSeason of
                    Just sn ->
                        discoverSeasonSection model title sn

                    Nothing ->
                        text ""
                ]
        ]


seasonOption : Maybe Api.SeriesSeason -> Api.SeriesSeason -> Html Msg
seasonOption sel sn =
    option
        [ value (String.fromInt sn.seasonNumber)
        , selected (Maybe.withDefault (-1) (Maybe.map .seasonNumber sel) == sn.seasonNumber)
        ]
        [ text (Maybe.withDefault ("SEASON " ++ String.fromInt sn.seasonNumber) sn.name) ]


discoverSeasonSection : Model -> String -> Api.SeriesSeason -> Html Msg
discoverSeasonSection model seriesName season =
    let
        heading =
            Maybe.withDefault ("SEASON " ++ String.fromInt season.seasonNumber) season.name
    in
    div []
        [ div [ class "flex items-center justify-between mb3" ]
            [ div [ class "f6 tracked-wider ttu fg-secondary" ] [ text heading ]
            , div
                [ class "req-btn tc f7 tracked-wide ttu pointer flex-none"
                , style "padding" "4px 12px"
                , onClick (DownloadSeason seriesName season.seasonNumber)
                ]
                [ text "SEASON PACK" ]
            ]
        , div [ class "flex flex-column bt b--default" ]
            (List.map (discoverEpisodeRow seriesName season.seasonNumber) season.episodes)
        ]


discoverEpisodeRow : String -> Int -> Api.SeriesEpisode -> Html Msg
discoverEpisodeRow seriesName season ep =
    let
        label =
            "S" ++ pad2 season ++ "E" ++ pad2 ep.episodeNumber
    in
    div [ class "req-row flex items-center gap-4 pv3 ph2 bb b--default" ]
        [ div
            [ class "flex-none f7 ttu tracked-wide fg-secondary"
            , style "min-width" "72px"
            ]
            [ text label ]
        , div [ class "flex-auto flex flex-column gap-1" ]
            [ div [ class "f5 fg-primary" ] [ text (Maybe.withDefault "(untitled)" ep.name) ]
            , div [ class "f7 fg-muted" ] [ text (Maybe.withDefault "" ep.airDate) ]
            ]
        , if ep.downloaded then
            div
                [ class "ba b--default b--dashed f7 tracked-wide ttu fg-muted flex-none"
                , style "padding" "4px 12px"
                ]
                [ text "DOWNLOADED" ]

          else
            div
                [ class "req-btn tc f7 tracked-wide ttu pointer flex-none"
                , style "padding" "6px 16px"
                , onClick (DownloadEpisode seriesName season ep.episodeNumber)
                ]
                [ text "DOWNLOAD" ]
        ]


pad2 : Int -> String
pad2 n =
    if n < 10 then
        "0" ++ String.fromInt n

    else
        String.fromInt n
