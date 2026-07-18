module Pages.Requests exposing (view)

import Catalog exposing (MediaType(..), Title)
import Components exposing (monogram)
import Dict exposing (Dict)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Request
import Types exposing (Model, Msg(..), Tab(..))


view : Model -> Html Msg
view model =
    div [ class "pa4" ]
        [ div [ class "flex items-center justify-between mb4" ]
            [ div [ class "text-xl ttu tracked-wide fg-primary" ] [ text "Requests" ]
            , button [ class "btn btn-primary", onClick (SetTab Discover) ] [ text "+ New Request" ]
            ]
        , div [ class "flex gap-5 bb b--default mb4" ]
            [ tab model.requestsTab "My Requests" Mine
            , tab model.requestsTab "Discover" Discover
            ]
        , case model.requestsTab of
            Mine ->
                viewMyRequests model.requests

            Discover ->
                viewDiscover model.requests
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


viewMyRequests : Dict Int Request.Entry -> Html Msg
viewMyRequests requests =
    div [ class "flex flex-column bt b--default" ]
        (List.map viewRequestRow (Request.requested requests))


viewRequestRow : { id : Int, entry : Request.Entry } -> Html Msg
viewRequestRow req =
    let
        title =
            Catalog.find req.id

        mediaLabel =
            case title.mediaType of
                Movie ->
                    "movie"

                Show ->
                    "show"
    in
    div [ class "req-row flex items-center gap-4 pv3 ph2 bb b--default" ]
        [ div
            [ class "flex-none ba b--default bg-panel flex items-center justify-center text-md fg-ghost b"
            , style "width" "48px"
            , style "height" "48px"
            ]
            [ text (Catalog.initial title) ]
        , div [ class "flex-auto flex flex-column gap-1" ]
            [ div [ class "f5 ttu tracked-wide" ] [ text title.title ]
            , div [ class "f7 fg-muted" ]
                [ text
                    (mediaLabel
                        ++ " · "
                        ++ String.fromInt title.year
                        ++ " · Requested "
                        ++ req.entry.date
                    )
                ]
            ]
        , viewStatusBadge req.entry.status
        ]


{-| Monochrome status badges: distinguished by border style + text tone only. -}
viewStatusBadge : Request.Status -> Html Msg
viewStatusBadge status =
    let
        tone =
            case status of
                Request.Available ->
                    "b--strong fg-primary"

                Request.Processing ->
                    "b--default fg-secondary"

                Request.Pending ->
                    "b--default b--dashed fg-muted"
    in
    div
        [ class "ba f7 tracked-wide ttu"
        , class tone
        , style "padding" "4px 12px"
        ]
        [ text (Request.statusLabel status) ]


viewDiscover : Dict Int Request.Entry -> Html Msg
viewDiscover requests =
    div [ class "grid-fill" ]
        (List.map discoverCard (Request.discoverable requests Catalog.catalog))


discoverCard : Title -> Html Msg
discoverCard title =
    div [ class "relative ba b--default bg-panel overflow-hidden", style "height" "240px" ]
        [ monogram "64px" title
        , div
            [ class "absolute f7 fg-muted"
            , style "top" "8px"
            , style "right" "8px"
            ]
            [ text (String.fromInt title.year) ]
        , div [ class "absolute left-0 right-0 bottom-0 pa2 flex flex-column gap-2 fade-up-strong" ]
            [ div [ class "f7 ttu tracked-wide" ] [ text title.title ]
            , div
                [ class "req-btn tc f7 tracked-wide ttu"
                , style "padding" "6px 0"
                , onClick (RequestTitle title.id)
                ]
                [ text "+ Request" ]
            ]
        ]
