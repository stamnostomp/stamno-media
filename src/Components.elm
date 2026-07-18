module Components exposing
    ( cardTitle
    , continueWatchingCard
    , formatBytes
    , libraryCard
    , libraryRowCard
    , monogram
    )

import Api
import Catalog exposing (Title)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Types exposing (Msg(..))


monogram : String -> Title -> Html Msg
monogram size title =
    div
        [ class "absolute absolute--fill flex items-center justify-center fg-ghost b"
        , style "font-size" size
        ]
        [ text (Catalog.initial title) ]


cardTitle : Title -> Html Msg
cardTitle title =
    div [ class "w-100 pa2 fade-up f7 ttu tracked-wide" ] [ text title.title ]


formatBytes : Int -> String
formatBytes bytes =
    let
        mb =
            toFloat bytes / 1048576

        roundTo1 x =
            toFloat (round (x * 10)) / 10
    in
    if mb >= 1024 then
        String.fromFloat (roundTo1 (mb / 1024)) ++ " GB"

    else
        String.fromFloat (roundTo1 mb) ++ " MB"


libraryCard : Api.Item -> Html Msg
libraryCard item =
    div
        [ class "poster relative pointer"
        , style "height" "240px"
        , onClick (OpenRealDetail item)
        ]
        [ div
            [ class "absolute absolute--fill flex items-center justify-center fg-ghost b"
            , style "font-size" "64px"
            ]
            [ text (String.left 1 item.title) ]
        , div [ class "reveal absolute absolute--fill flex flex-column justify-between" ]
            [ div [ class "pa2 tr f7 fg-muted" ] [ text (formatBytes item.size) ]
            , div [ class "w-100 pa2 fade-up f7 ttu tracked-wide" ] [ text item.title ]
            ]
        ]


{-| Fixed-width variant of 'libraryCard' for horizontal row-scrolls. -}
libraryRowCard : Api.Item -> Html Msg
libraryRowCard item =
    div
        [ class "poster relative pointer flex-none"
        , style "width" "160px"
        , style "height" "240px"
        , onClick (OpenRealDetail item)
        ]
        [ div
            [ class "absolute absolute--fill flex items-center justify-center fg-ghost b"
            , style "font-size" "64px"
            ]
            [ text (String.left 1 item.title) ]
        , div [ class "reveal absolute absolute--fill flex items-end" ]
            [ div [ class "w-100 pa2 fade-up f7 ttu tracked-wide" ] [ text item.title ] ]
        ]


continueWatchingCard : Api.ContinueItem -> Html Msg
continueWatchingCard item =
    div
        [ class "poster relative pointer flex-none"
        , style "width" "160px"
        , style "height" "240px"
        , onClick
            (OpenRealDetail
                { id = item.id, title = item.title, size = item.size, mime = item.mime }
            )
        ]
        [ div
            [ class "absolute absolute--fill flex items-center justify-center fg-ghost b"
            , style "font-size" "64px"
            ]
            [ text (String.left 1 item.title) ]
        , div [ class "reveal absolute absolute--fill flex items-end" ]
            [ div [ class "w-100 pa2 fade-up f7 ttu tracked-wide" ] [ text item.title ] ]
        , div
            [ class "absolute left-0 bottom-0"
            , style "height" "3px"
            , style "width" (String.fromInt item.progress ++ "%")
            , style "background" "var(--gray-5)"
            ]
            []
        ]
