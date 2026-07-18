module Pages.Detail exposing (view)

import Catalog exposing (Title)
import Components exposing (cardTitle, monogram)
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Types exposing (Msg(..), Screen(..))


view : Title -> Html Msg
view active =
    div []
        [ div
            [ class "pointer pv3 ph4 f6 fg-secondary tracked-wide"
            , onClick (NavigateTo Home)
            ]
            [ text "← BACK" ]
        , div [ class "relative z-0 backdrop-bg bb b--default", style "height" "340px" ] []
        , div
            [ class "relative z-1 flex gap-6 ph4"
            , style "margin-top" "-120px"
            ]
            [ div
                [ class "relative ba b--strong bg-panel flex-none"
                , style "width" "200px"
                , style "height" "300px"
                ]
                [ monogram "96px" active ]
            , div
                [ class "flex-auto flex flex-column gap-4"
                , style "padding-top" "130px"
                , style "max-width" "700px"
                ]
                [ div [ class "f1 ttu b tracked-wide fg-primary" ] [ text active.title ]
                , div [ class "flex items-center gap-3 f6 fg-secondary" ]
                    [ span [] [ text (String.fromInt active.year) ]
                    , span [] [ text "·" ]
                    , span [] [ text active.duration ]
                    , span [] [ text "·" ]
                    , span [] [ text active.rating ]
                    ]
                , div [ class "flex gap-2" ]
                    [ span
                        [ class "inline-flex items-center ba b--default f7 ttu tracked-wide"
                        , style "height" "28px"
                        , style "padding" "0 12px"
                        ]
                        [ text (Catalog.genreLabel active.genre) ]
                    ]
                , div [ class "f5 fg-secondary lh-copy" ] [ text active.synopsis ]
                , div [ class "f6 fg-muted" ] [ text ("Cast: " ++ active.cast) ]
                , div [ class "flex gap-4 mt2" ]
                    [ button [ class "btn btn-primary", onClick OpenPlayer ] [ text "▶ Play" ]
                    , button [ class "btn btn-ghost" ] [ text "+ Add to List" ]
                    ]
                ]
            ]
        , div [ class "ph4 pv5" ]
            [ div [ class "f6 tracked-wider ttu fg-secondary mb3" ] [ text "More Like This" ]
            , div [ class "row-scroll flex gap-4" ]
                (List.map relatedCard (Catalog.related active.id))
            ]
        ]


relatedCard : Title -> Html Msg
relatedCard title =
    div
        [ class "poster relative pointer flex-none"
        , style "width" "160px"
        , style "height" "240px"
        , onClick (OpenDetail title.id)
        ]
        [ monogram "64px" title
        , div [ class "reveal absolute absolute--fill flex items-end" ] [ cardTitle title ]
        ]
