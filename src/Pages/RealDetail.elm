module Pages.RealDetail exposing (view)

import Api
import Components exposing (displayRowCard, formatBytes, groupForDisplay, posterImage)
import Html exposing (Html, button, div, form, input, span, text)
import Html.Attributes exposing (class, disabled, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Pages.Series
import Types exposing (Model, Msg(..))


view : Model -> Html Msg
view model =
    case model.activeRealItem of
        Nothing ->
            text ""

        -- m55: an episode card opens the SERIES detail (header + PLAY +
        -- season select + episode cards) instead of the single-item block
        Just item ->
            case item.seriesName of
                Just seriesName ->
                    seriesDetailView model item seriesName

                Nothing ->
                    itemDetailView model item


{-| m55: the series variant of the detail page. -}
seriesDetailView : Model -> Api.Item -> String -> Html Msg
seriesDetailView model item seriesName =
    let
        title =
            model.seriesDetail
                |> Maybe.andThen .name
                |> Maybe.withDefault seriesName

        mTarget =
            Pages.Series.playTargetEpisode model

        resume =
            case mTarget of
                Just t ->
                    model.continueWatching
                        |> List.filter (\c -> c.id == t.id)
                        |> List.head
                        |> Maybe.map (\c -> c.progress > 0 && c.progress < 100)
                        |> Maybe.withDefault False

                Nothing ->
                    False
    in
    div []
        [ div
            [ class "pointer pv3 ph4 f6 fg-secondary tracked-wide"
            , onClick (NavigateTo model.previousScreen)
            ]
            [ text "\u{2190} BACK" ]
        , div [ class "ph4" ]
            [ Pages.Series.seriesHeaderView model title ]
        , div [ class "flex gap-4 ph4 pb4 justify-end" ]
            [ if isAdmin model then
                button
                    [ class "btn"
                    , onClick (OpenMetaPicker item)
                    ]
                    [ text "METADATA" ]

              else
                text ""
            , button
                [ class "btn btn-primary"
                , disabled (mTarget == Nothing)
                , onClick (Maybe.withDefault (OpenRealPlayer item) (Maybe.map OpenRealPlayer mTarget))
                ]
                [ text
                    (if resume then
                        "\u{25B6} Resume"

                     else
                        "\u{25B6} Play"
                    )
                ]
            ]
        , if isMetaPickerFor model item then
            div [ class "ph4 pb4" ] [ metaPickerView model ]

          else
            text ""
        , div [ class "ph4 pb5" ]
            [ Pages.Series.seriesEpisodesView model title ]
        , div [ class "ph4 pb5" ]
            [ div [ class "f6 tracked-wider ttu fg-secondary mb3" ] [ text "More From Your Library" ]
            , div [ class "row-scroll flex gap-4" ]
                (model.library
                    |> List.filter (\i -> i.id /= item.id)
                    |> groupForDisplay
                    |> List.take 10
                    |> List.map (displayRowCard model.token model.library)
                )
            ]
        ]


itemDetailView : Model -> Api.Item -> Html Msg
itemDetailView model item =
            let
                maybeProgress =
                    model.continueWatching
                        |> List.filter (\c -> c.id == item.id)
                        |> List.head
                        |> Maybe.map .progress
            in
            div []
                [ div
                    [ class "pointer pv3 ph4 f6 fg-secondary tracked-wide"
                    , onClick (NavigateTo model.previousScreen)
                    ]
                    [ text "← BACK" ]
                , div
                    [ class "relative z-0 backdrop-bg bb b--default"
                    , style "height" "340px"
                    , style "background-image" ("url(" ++ Api.imageUrl model.token item.id "Backdrop" ++ ")")
                    , style "background-size" "cover"
                    , style "background-position" "center"
                    ]
                    []
                , div
                    [ class "relative z-1 flex gap-6 ph4"
                    , style "margin-top" "-120px"
                    ]
                    [ div
                        [ class "relative ba b--strong bg-panel flex-none flex items-center justify-center fg-ghost b overflow-hidden"
                        , style "width" "200px"
                        , style "height" "300px"
                        , style "font-size" "96px"
                        ]
                        [ text (String.left 1 item.title)
                        , posterImage model.token item.id
                        ]
                    , div
                        [ class "flex-auto flex flex-column gap-4"
                        , style "padding-top" "130px"
                        , style "max-width" "700px"
                        ]
                        [ div [ class "f1 ttu b tracked-wide fg-primary" ]
                            [ text (Maybe.withDefault item.title (metaField model .title)) ]
                        , div [ class "flex items-center gap-3 f6 fg-secondary flex-wrap" ]
                            ([ span [] [ text (formatBytes item.size) ]
                             , span [] [ text "·" ]
                             , span [] [ text item.mime ]
                             ]
                                ++ (case metaField model .year of
                                        Just y ->
                                            [ span [] [ text "·" ], span [] [ text (String.fromInt y) ] ]

                                        Nothing ->
                                            []
                                   )
                                ++ (case metaField model .rating of
                                        Just r ->
                                            [ span [] [ text "·" ], span [] [ text ("RATING " ++ String.fromFloat r) ] ]

                                        Nothing ->
                                            []
                                   )
                               )
                        , div [ class "flex items-center gap-2 flex-wrap" ]
                            (case model.itemMeta of
                                Just m ->
                                    List.map (\g -> span [ class "ba b--default f7 ttu tracked-wide fg-muted pa1" ] [ text g ]) m.genres

                                Nothing ->
                                    []
                            )
                        , div [ class "f5 fg-secondary lh-copy" ]
                            [ text
                                (case model.itemMeta of
                                    Just m ->
                                        case m.overview of
                                            Just o ->
                                                o

                                            Nothing ->
                                                fallbackDesc item

                                    Nothing ->
                                        fallbackDesc item
                                )
                            ]
                        , viewProgress maybeProgress
                        , div [ class "flex gap-4 mt2" ]
                            [ button
                                [ class "btn btn-primary"
                                , onClick (OpenRealPlayer item)
                                ]
                                [ text
                                    (if maybeProgress /= Nothing then
                                        "▶ Resume"

                                     else
                                        "▶ Play"
                                    )
                                ]
                            , if isAdmin model then
                                button
                                    [ class "btn"
                                    , onClick (OpenMetaPicker item)
                                    ]
                                    [ text "METADATA" ]

                              else
                                text ""
                            ]
                        , if isMetaPickerFor model item then
                            metaPickerView model

                          else
                            text ""
                        ]
                    ]
                , div [ class "ph4 pv5" ]
                    [ div [ class "f6 tracked-wider ttu fg-secondary mb3" ] [ text "More From Your Library" ]
                    , div [ class "row-scroll flex gap-4" ]
                        (model.library
                            |> List.filter (\i -> i.id /= item.id)
                            |> groupForDisplay
                            |> List.take 10
                            |> List.map (displayRowCard model.token model.library)
                        )
                    ]
                ]


metaField : Model -> (Api.ItemMetadata -> Maybe a) -> Maybe a
metaField m f =
    case m.itemMeta of
        Just meta ->
            f meta

        Nothing ->
            Nothing


fallbackDesc : Api.Item -> String
fallbackDesc it =
    it.title
        ++ " is a "
        ++ it.mime
        ++ " file ("
        ++ formatBytes it.size
        ++ ") from your media library."


viewProgress : Maybe Int -> Html Msg
viewProgress maybeProgress =
    case maybeProgress of
        Nothing ->
            text ""

        Just percent ->
            div [ class "flex flex-column gap-2", style "max-width" "300px" ]
                [ div [ class "f7 fg-muted" ] [ text (String.fromInt percent ++ "% watched") ]
                , div
                    [ class "relative w-100"
                    , style "height" "3px"
                    , style "background" "var(--gray-2)"
                    ]
                    [ div
                        [ class "absolute left-0 top-0 h-100"
                        , style "width" (String.fromInt percent ++ "%")
                        , style "background" "var(--white)"
                        ]
                        []
                    ]
                ]

isAdmin : Model -> Bool
isAdmin model =
    Maybe.map .isAdmin model.authUser == Just True


isMetaPickerFor : Model -> Api.Item -> Bool
isMetaPickerFor model item =
    case model.metaPickerFor of
        Just p ->
            p.id == item.id

        Nothing ->
            False


{-| m83: admin manual TMDB override panel - search TMDB, pick the right hit. -}
metaPickerView : Model -> Html Msg
metaPickerView model =
    div [ class "ba b--default bg-panel pa3 flex flex-column gap-3", style "max-width" "640px" ]
        [ div [ class "flex gap-2 items-center" ]
            [ Html.form [ class "flex gap-2 items-center flex-auto", onSubmit MetaSearchSubmit ]  -- m89: Enter submits
                [ input
                    [ class "f6 flex-auto"
                    , placeholder "SEARCH TMDB..."
                    , value model.metaQuery
                    , onInput SetMetaQuery
                    ]
                    []
                , button
                    [ class "btn btn-primary"
                    , disabled model.metaBusy
                    , type_ "submit"
                    ]
                    [ text (if model.metaBusy then "..." else "SEARCH") ]
                ]
            , button [ class "btn", onClick CloseMetaPicker ] [ text "CLOSE" ]
            ]
        , case model.metaNote of
            Just n ->
                div [ class "f7 fg-muted" ] [ text n ]

            Nothing ->
                text ""
        , div [ class "flex flex-column gap-2" ]
            (List.map metaResultRow model.metaResults)
        ]


metaResultRow : Api.RemoteSearchResult -> Html Msg
metaResultRow r =
    button
        [ class "btn ba b--default tl w-100"
        , onClick (PickTmdb r)
        ]
        [ text
            (r.name
                ++ (case r.year of
                        Just y ->
                            " (" ++ String.fromInt y ++ ")"

                        Nothing ->
                            ""
                   )
                ++ "  -  tmdb:" ++ String.fromInt r.tmdbId
            )
        ]
