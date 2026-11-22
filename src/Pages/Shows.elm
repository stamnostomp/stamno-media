module Pages.Shows exposing (view)

import Api
import Components exposing (DisplayItem(..), displayGridCard, groupForDisplay, isEpisodeItem)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import String
import Types exposing (Model, Msg(..))


{-| m43/m81/m85: the Shows tab renders ONE card per series via the universal
'DisplayItem' representation (groupForDisplay) - clicking it opens the
series page with the season box + episode list (m47). Searched by the
global search box (series name). -}
view : Model -> Html Msg
view model =
    let
        series =
            model.library
                |> List.filter isEpisodeItem
                |> groupForDisplay
                |> searchSeries model.search
    in
    div [ class "pa4" ]
        [ div [ class "text-xl ttu tracked-wide mb-5 fg-primary" ] [ text "SHOWS" ]
        , if List.isEmpty series then
            div [ class "f6 fg-muted" ] [ text "NO SHOWS YET" ]

          else
            div [ class "grid-fill" ] (List.map (displayGridCard model.token model.library) series)
        ]


searchSeries : String -> List DisplayItem -> List DisplayItem
searchSeries query groups =
    if String.isEmpty query then
        groups

    else
        let
            lower =
                String.toLower query
        in
        List.filter
            (\d ->
                case d of
                    DisplaySeries name _ ->
                        String.contains lower (String.toLower name)

                    DisplayMedia _ ->
                        False
            )
            groups
