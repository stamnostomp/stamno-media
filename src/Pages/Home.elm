module Pages.Home exposing (view)

import Api
import Components exposing (DisplayItem(..), continueWatchingCard, displayRowCard, groupForDisplay, posterImage)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onClick)
import String
import Types exposing (Model, Msg(..))


view : Model -> Html Msg
view model =
    div []
        [ viewHero model
        , div [ class "ph4 pt4 pb5 flex flex-column gap-7" ]
            [ viewContinueRow model
            , viewMaybeRow "Trending Now" "row-trending" (displayRowCard model.token model.library) (groupForDisplay model.popular)
            , viewMaybeRow "Recently Added" "row-recent" (displayRowCard model.token model.library) (groupForDisplay model.newItems)
            ]
        ]


{-| m41: the hero features the newest server item (GET /new head, falling
back to the first library item) instead of the hardcoded demo catalog. -}
heroItem : Model -> Maybe Api.Item
heroItem model =
    case model.newItems of
        (i :: _) ->
            Just i

        [] ->
            List.head model.library


{-| m89: the hero item has in-progress watch state (2%..95%). -}
heroInProgress : Model -> Api.Item -> Bool
heroInProgress model item =
    case List.head (List.filter (\c -> c.id == item.id) model.continueWatching) of
        Just c ->
            c.progress > 2 && c.progress < 95

        Nothing ->
            False


viewHero : Model -> Html Msg
viewHero model =
    let
        hero =
            heroItem model
    in
    div
        [ class "relative hero-bg bb b--default flex items-end pa-7"
        , style "height" "440px"
        ]
        (case hero of
            Nothing ->
                [ div [ class "flex flex-column gap-3", style "max-width" "640px" ]
                    [ div [ class "f1 ttu b tracked-wide fg-primary" ] [ text "STAMNO_MEDIA" ]
                    ]
                ]

            Just item ->
                [ div
                    [ class "absolute absolute--fill"
                    , style "background-image" ("url(" ++ Api.imageUrl model.token item.id "Backdrop" ++ ")")
                    , style "background-size" "cover"
                    , style "background-position" "center"
                    , style "opacity" "0.35"
                    ]
                    []
                , div [ class "relative flex flex-column gap-3", style "max-width" "640px" ]
                    [ div [ class "f1 ttu b tracked-wide fg-primary" ] [ text item.displayTitle ]
                    , div [ class "flex gap-4 mt3" ]
                        [ button [ class "btn btn-primary", onClick (OpenRealPlayer item) ]
                            [ text
                                -- m89: match the detail page's RESUME label
                                (if heroInProgress model item then
                                    "▶ Resume"

                                 else
                                    "▶ Play"
                                )
                            ]
                        , button [ class "btn btn-ghost", onClick (OpenRealDetail item) ] [ text "More Info" ]
                        ]
                    ]
                ]
        )


{-| m88: Continue Watching groups episodes by series via the universal
'DisplayItem' representation - one card per show (latest episode's poster,
its progress bar), opening the series page. Movies keep their own card. -}
viewContinueRow : Model -> Html Msg
viewContinueRow model =
    let
        entries =
            groupForDisplay (List.map continueToItem model.continueWatching)
    in
    if List.isEmpty entries then
        text ""

    else
        viewRow "Continue Watching" "row-continue" (continueCard model) entries


continueToItem : Api.ContinueItem -> Api.Item
continueToItem c =
    { id = c.id
    , title = c.title
    , size = c.size
    , mime = c.mime
    , displayTitle = c.displayTitle
    , itemType = c.itemType
    , seriesName = c.seriesName
    , itemSeason = c.itemSeason
    , itemEpisode = c.itemEpisode
    , genres = []
    , path = c.path
    }


continueCard : Model -> DisplayItem -> Html Msg
continueCard model d =
    case d of
        DisplayMedia item ->
            case List.head (List.filter (\c -> c.id == item.id) model.continueWatching) of
                Just c ->
                    continueWatchingCard model.token c

                Nothing ->
                    displayRowCard model.token model.library d

        DisplaySeries series eps ->
            case List.head eps of
                Nothing ->
                    text ""

                Just ep ->
                    let
                        prog =
                            model.continueWatching
                                |> List.filter (\c -> c.id == ep.id)
                                |> List.head
                                |> Maybe.map .progress
                    in
                    div
                        [ class "poster relative pointer flex-none"
                        , style "width" "160px"
                        , style "height" "240px"
                        , onClick (OpenSeries series)
                        ]
                        [ div
                            [ class "absolute absolute--fill flex items-center justify-center fg-ghost b"
                            , style "font-size" "64px"
                            ]
                            [ text (String.left 1 series) ]
                        , posterImage model.token ep.id
                        , div [ class "reveal absolute absolute--fill flex flex-column justify-between" ]
                            [ div [ class "pa2 tr f7 fg-muted" ] [ text "RESUME" ]
                            , div [ class "w-100 pa2 fade-up f7 ttu tracked-wide" ] [ text series ]
                            ]
                        , case prog of
                            Just p ->
                                div
                                    [ class "absolute left-0 bottom-0"
                                    , style "height" "3px"
                                    , style "width" (String.fromInt p ++ "%")
                                    , style "background" "var(--gray-5)"
                                    ]
                                    []

                            Nothing ->
                                text ""
                        ]


{-| Omits the row entirely (rather than showing an empty scroller) when
the server has nothing for it yet, e.g. "Continue Watching" before
anything has been played.
-}
viewMaybeRow : String -> String -> (a -> Html Msg) -> List a -> Html Msg
viewMaybeRow label rowId card items =
    if List.isEmpty items then
        text ""

    else
        viewRow label rowId card items


viewRow : String -> String -> (a -> Html Msg) -> List a -> Html Msg
viewRow label rowId card items =
    div []
        [ div [ class "flex items-center justify-between mb3" ]
            [ div [ class "f6 tracked-wider ttu fg-secondary" ] [ text label ]
            , div [ class "flex gap-2" ]
                [ div [ class "arrow-btn", onClick (ScrollRow rowId -520) ] [ text "‹" ]
                , div [ class "arrow-btn", onClick (ScrollRow rowId 520) ] [ text "›" ]
                ]
            ]
        , div [ id rowId, class "row-scroll flex gap-4 pb2" ] (List.map card items)
        ]
