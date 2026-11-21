module Components exposing
    ( cardTitle
    , continueWatchingCard
    , formatBytes
    , libraryCard
    , libraryRowCard
    , monogram
    , posterImage
    )

import Api
import Catalog exposing (Title)
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (onClick)
import Types exposing (Msg(..))


{-| m34: server poster artwork layered ON TOP of the letter placeholder
(if the image 404s the monogram still shows through). -}
posterImage : Maybe String -> String -> Html Msg
posterImage token id =
    img
        [ class "absolute absolute--fill w-100 h-100"
        , style "object-fit" "cover"
        , src (Api.imageUrl token id "Primary")
        ]
        []


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


libraryCard : Maybe String -> Api.Item -> Html Msg
libraryCard token item =
    div
        [ class "poster relative pointer"
        , style "height" "240px"
        , onClick (OpenRealDetail item)
        ]
        [ div
            [ class "absolute absolute--fill flex items-center justify-center fg-ghost b"
            , style "font-size" "64px"
            ]
            [ text (String.left 1 item.displayTitle) ]
        , posterImage token item.id
        , div [ class "reveal absolute absolute--fill flex flex-column justify-between" ]
            [ div [ class "pa2 tr f7 fg-muted" ] [ text (formatBytes item.size) ]
            , div [ class "w-100 pa2 fade-up f7 ttu tracked-wide" ] [ text (cardLabel item) ]
            ]
        ]


{-| m81: episode cards are labelled "S01E05 · Name" (not the series name,
-- which made every episode look like its own show); everything else uses
-- the display title. -}
cardLabel : Api.Item -> String
cardLabel item =
    Maybe.withDefault item.displayTitle (Api.episodeLabel item)


{-| Fixed-width variant of 'libraryCard' for horizontal row-scrolls. -}
libraryRowCard : Maybe String -> Api.Item -> Html Msg
libraryRowCard token item =
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
            [ text (String.left 1 item.displayTitle) ]
        , posterImage token item.id
        , div [ class "reveal absolute absolute--fill flex items-end" ]
            [ div [ class "w-100 pa2 fade-up f7 ttu tracked-wide" ] [ text (cardLabel item) ] ]
        ]


continueWatchingCard : Maybe String -> Api.ContinueItem -> Html Msg
continueWatchingCard token item =
    div
        [ class "poster relative pointer flex-none"
        , style "width" "160px"
        , style "height" "240px"
        , onClick
            (OpenRealDetail
                { id = item.id, title = item.title, size = item.size, mime = item.mime, displayTitle = item.displayTitle, itemType = item.itemType, seriesName = item.seriesName, itemSeason = item.itemSeason, itemEpisode = item.itemEpisode, genres = [], path = item.path }
            )
        ]
        [ div
            [ class "absolute absolute--fill flex items-center justify-center fg-ghost b"
            , style "font-size" "64px"
            ]
            [ text (String.left 1 item.displayTitle) ]
        , posterImage token item.id
        , div [ class "reveal absolute absolute--fill flex items-end" ]
            [ div [ class "w-100 pa2 fade-up f7 ttu tracked-wide" ] [ text item.displayTitle ] ]
        , div
            [ class "absolute left-0 bottom-0"
            , style "height" "3px"
            , style "width" (String.fromInt item.progress ++ "%")
            , style "background" "var(--gray-5)"
            ]
            []
        ]
