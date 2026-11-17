module Pages.Home exposing (view)

import Catalog exposing (Title)
import Components exposing (continueWatchingCard, libraryRowCard)
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onClick)
import Types exposing (Model, Msg(..))


view : Model -> Html Msg
view model =
    div []
        [ viewHero (Catalog.find model.activeId)
        , div [ class "ph4 pt4 pb5 flex flex-column gap-7" ]
            [ viewMaybeRow "Continue Watching" "row-continue" continueWatchingCard model.continueWatching
            , viewMaybeRow "Trending Now" "row-trending" libraryRowCard model.popular
            , viewMaybeRow "Recently Added" "row-recent" libraryRowCard model.newItems
            ]
        ]


viewHero : Title -> Html Msg
viewHero active =
    div
        [ class "relative hero-bg bb b--default flex items-end pa-7"
        , style "height" "440px"
        ]
        [ div [ class "flex flex-column gap-3", style "max-width" "640px" ]
            [ div [ class "f1 ttu b tracked-wide fg-primary" ] [ text active.title ]
            , div [ class "flex items-center gap-3 f6 fg-secondary" ]
                [ span [] [ text (String.fromInt active.year) ]
                , span [] [ text "·" ]
                , span [] [ text active.duration ]
                , span [] [ text "·" ]
                , span
                    [ class "ba b--default ttu tracked-wide"
                    , style "padding" "2px 8px"
                    ]
                    [ text (Catalog.genreLabel active.genre) ]
                ]
            , div [ class "f5 fg-secondary lh-copy" ] [ text active.synopsis ]
            , div [ class "flex gap-4 mt3" ]
                [ button [ class "btn btn-primary", onClick OpenPlayer ] [ text "▶ Play" ]
                , button [ class "btn btn-ghost", onClick (OpenDetail active.id) ] [ text "More Info" ]
                ]
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
