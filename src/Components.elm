module Components exposing
    ( DisplayItem(..)
    , cardTitle
    , continueWatchingCard
    , displayGridCard
    , displayRowCard
    , formatBytes
    , groupForDisplay
    , isEpisodeItem
    , libraryCard
    , libraryRowCard
    , monogram
    , posterImage
    , seriesEpisodeCount
    )

import Api
import Dict
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


{-| m85: universal display representation. A show is ALWAYS a single
'DisplaySeries' entry carrying its episodes inside, so every list/row/grid
renders it as one card (opening the series page) - never as loose episode
cards. Movies and other items are 'DisplayMedia'. -}
type DisplayItem
    = DisplayMedia Api.Item
    | DisplaySeries String (List Api.Item)


isEpisodeItem : Api.Item -> Bool
isEpisodeItem i =
    String.toLower i.itemType == "episode" || i.seriesName /= Nothing


{-| Collapse episodes into their series group at the position of the
first (newest, for recency-sorted lists) episode; non-episodes pass through
in order. -}
groupForDisplay : List Api.Item -> List DisplayItem
groupForDisplay items =
    let
        step item ( dict, acc ) =
            if isEpisodeItem item then
                let
                    key =
                        Maybe.withDefault "OTHER" item.seriesName
                in
                case Dict.get key dict of
                    Just _ ->
                        ( Dict.update key (Maybe.map (\eps -> eps ++ [ item ])) dict, acc )

                    Nothing ->
                        ( Dict.insert key [ item ] dict, acc ++ [ DisplaySeries key [ item ] ] )

            else
                ( dict, acc ++ [ DisplayMedia item ] )
    in
    Tuple.second (List.foldl step ( Dict.empty, [] ) items)


{-| Total episodes of the series in the whole library, for the badge. -}
seriesEpisodeCount : List Api.Item -> String -> Int
seriesEpisodeCount library series =
    List.length (List.filter (\i -> isEpisodeItem i && i.seriesName == Just series) library)


epCountBadge : Int -> Html Msg
epCountBadge count =
    div [ class "pa2 tr f7 fg-muted" ]
        [ text (String.fromInt count ++ (if count == 1 then " EPISODE" else " EPISODES")) ]


{-| Row-scroll card (160x240) for a 'DisplayItem': media opens its detail
page, a series opens the series page. -}
displayRowCard : Maybe String -> List Api.Item -> DisplayItem -> Html Msg
displayRowCard token library d =
    case d of
        DisplayMedia item ->
            libraryRowCard token item

        DisplaySeries series eps ->
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
                            [ epCountBadge (seriesEpisodeCount library series)
                            , div [ class "w-100 pa2 fade-up f7 ttu tracked-wide" ] [ text series ]
                            ]
                        ]


{-| Grid card for a 'DisplayItem' (Library page, Shows page). -}
displayGridCard : Maybe String -> List Api.Item -> DisplayItem -> Html Msg
displayGridCard token library d =
    case d of
        DisplayMedia item ->
            libraryCard token item

        DisplaySeries series eps ->
            case List.head eps of
                Nothing ->
                    text ""

                Just ep ->
                    div
                        [ class "poster relative pointer"
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
                            [ epCountBadge (seriesEpisodeCount library series)
                            , div [ class "w-100 pa2 fade-up f7 ttu tracked-wide" ] [ text series ]
                            ]
                        ]
