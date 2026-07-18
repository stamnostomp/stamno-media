module Pages.RealDetail exposing (view)

import Components exposing (formatBytes, libraryRowCard)
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Types exposing (Model, Msg(..))


view : Model -> Html Msg
view model =
    case model.activeRealItem of
        Nothing ->
            text ""

        Just item ->
            let
                maybeProgress =
                    model.continueWatching
                        |> List.filter (\c -> c.id == item.id)
                        |> List.head
                        |> Maybe.map .progress
            in
            div []
                [ div
                    [ class "pointer pv3 ph4 f6 fg-secondary tracked-wide"
                    , onClick (NavigateTo model.previousScreen)
                    ]
                    [ text "← BACK" ]
                , div [ class "relative z-0 backdrop-bg bb b--default", style "height" "340px" ] []
                , div
                    [ class "relative z-1 flex gap-6 ph4"
                    , style "margin-top" "-120px"
                    ]
                    [ div
                        [ class "relative ba b--strong bg-panel flex-none flex items-center justify-center fg-ghost b"
                        , style "width" "200px"
                        , style "height" "300px"
                        , style "font-size" "96px"
                        ]
                        [ text (String.left 1 item.title) ]
                    , div
                        [ class "flex-auto flex flex-column gap-4"
                        , style "padding-top" "130px"
                        , style "max-width" "700px"
                        ]
                        [ div [ class "f1 ttu b tracked-wide fg-primary" ] [ text item.title ]
                        , div [ class "flex items-center gap-3 f6 fg-secondary" ]
                            [ span [] [ text (formatBytes item.size) ]
                            , span [] [ text "·" ]
                            , span [] [ text item.mime ]
                            ]
                        , div [ class "f5 fg-secondary lh-copy" ]
                            [ text
                                (item.title
                                    ++ " is a "
                                    ++ item.mime
                                    ++ " file ("
                                    ++ formatBytes item.size
                                    ++ ") from your media library."
                                )
                            ]
                        , viewProgress maybeProgress
                        , div [ class "flex gap-4 mt2" ]
                            [ button
                                [ class "btn btn-primary"
                                , onClick (OpenRealPlayer item)
                                ]
                                [ text
                                    (if maybeProgress /= Nothing then
                                        "▶ Resume"

                                     else
                                        "▶ Play"
                                    )
                                ]
                            ]
                        ]
                    ]
                , div [ class "ph4 pv5" ]
                    [ div [ class "f6 tracked-wider ttu fg-secondary mb3" ] [ text "More From Your Library" ]
                    , div [ class "row-scroll flex gap-4" ]
                        (model.library
                            |> List.filter (\i -> i.id /= item.id)
                            |> List.take 10
                            |> List.map libraryRowCard
                        )
                    ]
                ]


viewProgress : Maybe Int -> Html Msg
viewProgress maybeProgress =
    case maybeProgress of
        Nothing ->
            text ""

        Just percent ->
            div [ class "flex flex-column gap-2", style "max-width" "300px" ]
                [ div [ class "f7 fg-muted" ] [ text (String.fromInt percent ++ "% watched") ]
                , div
                    [ class "relative w-100"
                    , style "height" "3px"
                    , style "background" "var(--gray-2)"
                    ]
                    [ div
                        [ class "absolute left-0 top-0 h-100"
                        , style "width" (String.fromInt percent ++ "%")
                        , style "background" "var(--white)"
                        ]
                        []
                    ]
                ]
