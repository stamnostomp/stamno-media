module View exposing (view)

import Browser
import Catalog exposing (Title)
import Html exposing (Html, div, input, span, text)
import Html.Attributes exposing (class, height, style, width)
import Html.Events exposing (onClick, onInput)
import Math.Vector2 as Vec2
import Pages.Detail
import Pages.Home
import Pages.Library
import Pages.Movies
import Pages.Player
import Pages.RealDetail
import Pages.Requests
import Pages.Shows
import Shaders.Background
import Shaders.Mesh
import Types exposing (Model, Msg(..), Screen(..))
import WebGL


view : Model -> Browser.Document Msg
view model =
    { title = "STAMNO_MEDIA"
    , body =
        [ viewBackground model
        , div [ class "relative z-1 min-vh-100 fg-primary overflow-x-hidden" ]
            [ if model.screen == Player then
                text ""

              else
                viewHeader model.screen model.search
            , viewScreen model
            ]
        ]
    }


{-| Full-page background shader, fixed behind all content. -}
viewBackground : Model -> Html Msg
viewBackground model =
    WebGL.toHtml
        [ width (round model.resolution.width)
        , height (round model.resolution.height)
        , class "fixed top-0 left-0 z-0"
        , style "width" "100vw"
        , style "height" "100vh"
        , style "pointer-events" "none"
        ]
        [ WebGL.entity
            Shaders.Mesh.vertexShader
            Shaders.Background.fragmentShader
            Shaders.Mesh.fullscreenMesh
            { time = model.time
            , resolution = Vec2.vec2 model.resolution.width model.resolution.height
            , mousePosition = Vec2.vec2 0 0
            , hoveredBranch = -1.0
            , centerPosition = Vec2.vec2 0 0
            }
        ]


viewScreen : Model -> Html Msg
viewScreen model =
    case model.screen of
        Home ->
            Pages.Home.view model

        Movies ->
            Pages.Movies.view model

        Shows ->
            Pages.Shows.view model

        Library ->
            Pages.Library.view "LIBRARY" model

        Detail ->
            Pages.Detail.view (Catalog.find model.activeId)

        RealDetail ->
            Pages.RealDetail.view model

        Player ->
            Pages.Player.view model

        Requests ->
            Pages.Requests.view model


viewHeader : Screen -> String -> Html Msg
viewHeader screen search =
    let
        showResults =
            screen == Home && not (String.isEmpty search)

        results =
            Catalog.searchBy search Catalog.catalog
    in
    div [ class "sticky-top flex items-center justify-between ph4 bb b--default bg-page", style "height" "64px" ]
        [ div [ class "flex items-center gap-6" ]
            [ div
                [ class "pointer b text-md tracked-wider ttu"
                , onClick (NavigateTo Home)
                ]
                [ text "STAMNO_MEDIA" ]
            , div [ class "flex gap-5" ]
                (List.map (navLink screen)
                    [ ( "Home", Home )
                    , ( "Movies", Movies )
                    , ( "Shows", Shows )
                    , ( "Library", Library )
                    , ( "Requests", Requests )
                    ]
                )
            ]
        , div [ class "flex items-center gap-5" ]
            [ div [ class "relative" ]
                [ input
                    [ class "f6 tracked-wide"
                    , style "width" "180px"
                    , style "padding" "4px 8px"
                    , Html.Attributes.placeholder "SEARCH$"
                    , Html.Attributes.value search
                    , onInput SetSearch
                    ]
                    []
                , if showResults then
                    viewSearchResults results

                  else
                    text ""
                ]
            , div [ class "ba b--default bg-panel", style "width" "28px", style "height" "28px" ] []
            ]
        ]


viewSearchResults : List Title -> Html Msg
viewSearchResults results =
    if List.isEmpty results then
        div [ class "search-dropdown pa3 f7 fg-muted tracked-wide" ] [ text "NO RESULTS" ]

    else
        div [ class "search-dropdown" ]
            (List.map searchResultItem results)


searchResultItem : Title -> Html Msg
searchResultItem title =
    div
        [ class "pointer flex items-center gap-3 pa2"
        , onClick (OpenDetail title.id)
        ]
        [ span [ class "fg-ghost b" ] [ text (Catalog.initial title) ]
        , span [ class "f7 ttu tracked-wide fg-primary" ] [ text title.title ]
        , span [ class "f7 fg-muted ml-auto" ] [ text (String.fromInt title.year) ]
        ]


navLink : Screen -> ( String, Screen ) -> Html Msg
navLink current ( label, target ) =
    div
        [ class "pointer f6 tracked-wide ttu bb"
        , class
            (if current == target then
                "fg-primary b--strong"

             else
                "fg-secondary b--trans"
            )
        , style "padding-bottom" "2px"
        , onClick (NavigateTo target)
        ]
        [ text label ]
