module Pages.Shows exposing (view)

import Catalog exposing (Genre(..), Title)
import Components exposing (cardTitle, monogram)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Types exposing (Model, Msg(..))


view : Model -> Html Msg
view model =
    let
        results =
            Catalog.shows Catalog.catalog
                |> Catalog.byGenre model.filter
                |> Catalog.searchBy model.search
    in
    div [ class "pa4" ]
        [ div [ class "text-xl ttu tracked-wide mb-5 fg-primary" ] [ text "Shows" ]
        , viewFilterChips model.filter
        , div [ class "grid-fill" ] (List.map gridCard results)
        ]


viewFilterChips : Maybe Genre -> Html Msg
viewFilterChips active =
    let
        chip ( label, filter ) =
            div
                [ class "pointer ba f7 tracked-wide ttu"
                , class
                    (if active == filter then
                        "b--strong fg-primary"

                     else
                        "b--default fg-secondary"
                    )
                , style "padding" "6px 16px"
                , onClick (SetFilter filter)
                ]
                [ text label ]
    in
    div [ class "flex flex-wrap gap-3 mb4" ]
        (List.map chip
            [ ( "ALL", Nothing )
            , ( "ACTION", Just Action )
            , ( "SCIFI", Just Scifi )
            , ( "DRAMA", Just Drama )
            , ( "THRILLER", Just Thriller )
            ]
        )


gridCard : Title -> Html Msg
gridCard title =
    div
        [ class "poster relative pointer"
        , style "height" "240px"
        , onClick (OpenDetail title.id)
        ]
        [ monogram "64px" title
        , div [ class "reveal absolute absolute--fill flex flex-column justify-between" ]
            [ div [ class "pa2 tr f7 fg-muted" ] [ text title.duration ]
            , cardTitle title
            ]
        ]
