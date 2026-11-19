module Pages.Shows exposing (view)

import Api
import Components exposing (libraryCard)
import Dict
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Types exposing (Model, Msg(..))


{-| m43: real episode items grouped by series name; searched by the global
search box. When the library has no episodes, a muted note shows. -}
view : Model -> Html Msg
view model =
    let
        episodes =
            model.library
                |> List.filter isEpisode
                |> search model.search
                |> groupBySeries
    in
    div [ class "pa4" ]
        [ div [ class "text-xl ttu tracked-wide mb-5 fg-primary" ] [ text "SHOWS" ]
        , if List.isEmpty episodes then
            div [ class "f6 fg-muted" ] [ text "NO SHOWS YET" ]

          else
            div [ class "flex flex-column gap-7" ]
                (List.map (seriesSection model.token) episodes)
        ]


isEpisode : Api.Item -> Bool
isEpisode i =
    String.toLower i.itemType == "episode" || i.seriesName /= Nothing


seriesSection : Maybe String -> ( String, List Api.Item ) -> Html Msg
seriesSection token ( series, eps ) =
    div []
        [ -- m47: clicking the series header opens the series view
          div
            [ class "f6 tracked-wider ttu fg-secondary mb3 pointer b"
            , onClick (OpenSeries series)
            ]
            [ text series ]
        , div [ class "grid-fill" ] (List.map (libraryCard token) eps)
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


search : String -> List Api.Item -> List Api.Item
search query items =
    if String.isEmpty query then
        items

    else
        let
            lower =
                String.toLower query
        in
        List.filter (\item -> String.contains lower (String.toLower item.displayTitle) || String.contains lower (String.toLower item.title)) items
