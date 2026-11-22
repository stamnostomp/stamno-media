module Pages.Home exposing (view)

import Api
import Components exposing (continueWatchingCard, displayRowCard, groupForDisplay)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onClick)
import Types exposing (Model, Msg(..))


view : Model -> Html Msg
view model =
    div []
        [ viewHero model
        , div [ class "ph4 pt4 pb5 flex flex-column gap-7" ]
            [ viewMaybeRow "Continue Watching" "row-continue" (continueWatchingCard model.token) model.continueWatching
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
