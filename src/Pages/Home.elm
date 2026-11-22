module Pages.Home exposing (view)

import Api
import Components exposing (continueWatchingCard, libraryRowCard, posterImage)
import Dict
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
            [ viewMaybeRow "Continue Watching" "row-continue" (continueWatchingCard model.token) model.continueWatching
            , viewMaybeRow "Trending Now" "row-trending" (libraryRowCard model.token) model.popular
            , viewRecentRow model
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
                        [ button [ class "btn btn-primary", onClick (OpenRealPlayer item) ] [ text "Play" ]
                        , button [ class "btn btn-ghost", onClick (OpenRealDetail item) ] [ text "More Info" ]
                        ]
                    ]
                ]
        )


{-| m84: "Recently Added" groups episodes by series - one card per series
(click opens the series page, like the Shows tab) instead of one card per
episode. Movies and other items stay individual cards. -}
viewRecentRow : Model -> Html Msg
viewRecentRow model =
    let
        entries =
            groupRecent model.newItems
    in
    if List.isEmpty entries then
        text ""

    else
        viewRow "Recently Added" "row-recent" (recentCard model.token model.library) entries


type RecentEntry
    = RecentItem Api.Item
    | RecentSeries String (List Api.Item)


recentCard : Maybe String -> List Api.Item -> RecentEntry -> Html Msg
recentCard token library entry =
    case entry of
        RecentItem item ->
            libraryRowCard token item

        RecentSeries series eps ->
            recentSeriesCard token series eps (seriesEpisodeCount library series)


{-| Collapse episodes into their series group at the position of the
newest episode; non-episodes pass through. -}
groupRecent : List Api.Item -> List RecentEntry
groupRecent items =
    let
        step item ( dict, acc ) =
            if isEpisode item then
                let
                    key =
                        Maybe.withDefault "OTHER" item.seriesName
                in
                case Dict.get key dict of
                    Just _ ->
                        ( Dict.update key (Maybe.map (\eps -> eps ++ [ item ])) dict, acc )

                    Nothing ->
                        ( Dict.insert key [ item ] dict, acc ++ [ RecentSeries key [ item ] ] )

            else
                ( dict, acc ++ [ RecentItem item ] )
    in
    Tuple.second (List.foldl step ( Dict.empty, [] ) items)


isEpisode : Api.Item -> Bool
isEpisode i =
    String.toLower i.itemType == "episode" || i.seriesName /= Nothing


{-| Total episodes of the series in the whole library (not just the /new
window), so the badge is the real count. -}
seriesEpisodeCount : List Api.Item -> String -> Int
seriesEpisodeCount library series =
    List.length (List.filter (\i -> isEpisode i && i.seriesName == Just series) library)


{-| Row-sized series card: poster of the newest episode, episode count on
top, series name at the bottom; click opens the series page. -}
recentSeriesCard : Maybe String -> String -> List Api.Item -> Int -> Html Msg
recentSeriesCard token series eps count =
    case List.head eps of
        Nothing ->
            text ""

        Just ep ->
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
                , posterImage token ep.id
                , div [ class "reveal absolute absolute--fill flex flex-column justify-between" ]
                    [ div [ class "pa2 tr f7 fg-muted" ] [ text (String.fromInt count ++ (if count == 1 then " EPISODE" else " EPISODES")) ]
                    , div [ class "w-100 pa2 fade-up f7 ttu tracked-wide" ] [ text series ]
                    ]
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
