module Pages.Shows exposing (view)

import Api
import Components exposing (posterImage)
import Dict
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import String
import Types exposing (Model, Msg(..))


{-| m43/m81: real episode items grouped by series name. m81: the Shows tab
renders ONE card per series (like a movie card) - clicking it opens the
series page with the season box + episode list (m47) - instead of listing
every episode card inline. Searched by the global search box (series name). -}
view : Model -> Html Msg
view model =
    let
        series =
            model.library
                |> List.filter isEpisode
                |> groupBySeries
                |> searchSeries model.search
    in
    div [ class "pa4" ]
        [ div [ class "text-xl ttu tracked-wide mb-5 fg-primary" ] [ text "SHOWS" ]
        , if List.isEmpty series then
            div [ class "f6 fg-muted" ] [ text "NO SHOWS YET" ]

          else
            div [ class "grid-fill" ] (List.map (seriesCard model.token) series)
        ]


isEpisode : Api.Item -> Bool
isEpisode i =
    String.toLower i.itemType == "episode" || i.seriesName /= Nothing


{-| One poster card per series; the poster is the first episode's image. -}
seriesCard : Maybe String -> ( String, List Api.Item ) -> Html Msg
seriesCard token ( series, eps ) =
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
                    [ div [ class "pa2 tr f7 fg-muted" ] [ text (String.fromInt (List.length eps) ++ " EPISODES") ]
                    , div [ class "w-100 pa2 fade-up f7 ttu tracked-wide" ] [ text series ]
                    ]
                ]


{-| Group episodes by series name (missing -> OTHER), preserving order. -}
groupBySeries : List Api.Item -> List ( String, List Api.Item )
groupBySeries items =
    let
        add i dict =
            let
                key =
                    Maybe.withDefault "OTHER" i.seriesName
            in
            Dict.insert key (Maybe.withDefault [] (Dict.get key dict) ++ [ i ]) dict
    in
    Dict.toList (List.foldl add Dict.empty items)


searchSeries : String -> List ( String, List Api.Item ) -> List ( String, List Api.Item )
searchSeries query groups =
    if String.isEmpty query then
        groups

    else
        let
            lower =
                String.toLower query
        in
        List.filter (\( name, _ ) -> String.contains lower (String.toLower name)) groups
