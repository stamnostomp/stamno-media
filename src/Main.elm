port module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events
import Catalog exposing (Genre(..), MediaType(..), Title)
import Dict exposing (Dict)
import Html exposing (Html, button, div, input, span, text)
import Html.Attributes exposing (class, height, id, style, width)
import Html.Events exposing (onClick, onInput)
import Math.Vector2 as Vec2
import Request
import Shaders.Background
import Shaders.Mesh
import Task
import WebGL



-- PORTS


port scrollRow : { rowId : String, delta : Float } -> Cmd msg



-- MODEL


type Screen
    = Home
    | Movies
    | Shows
    | Library
    | Detail
    | Player
    | Requests


type Tab
    = Mine
    | Discover


type alias Model =
    { screen : Screen
    , activeId : Int
    , filter : Maybe Genre
    , search : String
    , requestsTab : Tab
    , requests : Dict Int Request.Entry
    , time : Float
    , resolution : { width : Float, height : Float }
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { screen = Home
      , activeId = 1
      , filter = Nothing
      , search = ""
      , requestsTab = Mine
      , requests = Request.initialEntries
      , time = 0
      , resolution = { width = 1920, height = 1080 }
      }
    , Task.perform GotViewport Dom.getViewport
    )



-- UPDATE


type Msg
    = NavigateTo Screen
    | OpenDetail Int
    | OpenPlayer
    | BackFromPlayer
    | SetFilter (Maybe Genre)
    | SetSearch String
    | SetTab Tab
    | RequestTitle Int
    | ScrollRow String Float
    | Tick Float
    | GotViewport Dom.Viewport
    | WindowResized Int Int
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NavigateTo screen ->
            ( { model | screen = screen }, Cmd.none )

        OpenDetail titleId ->
            ( { model | screen = Detail, activeId = titleId }, Cmd.none )

        OpenPlayer ->
            ( { model | screen = Player }, Cmd.none )

        BackFromPlayer ->
            ( { model | screen = Detail }, Cmd.none )

        SetFilter filter ->
            ( { model | filter = filter }, Cmd.none )

        SetSearch query ->
            ( { model | search = query }, Cmd.none )

        SetTab tab ->
            ( { model | requestsTab = tab }, Cmd.none )

        RequestTitle titleId ->
            ( { model | requests = Request.request titleId model.requests }, Cmd.none )

        ScrollRow rowId delta ->
            ( model, scrollRow { rowId = rowId, delta = delta } )

        Tick delta ->
            ( { model | time = model.time + delta * 0.001 }, Cmd.none )

        GotViewport vp ->
            ( { model | resolution = { width = vp.viewport.width, height = vp.viewport.height } }
            , Cmd.none
            )

        WindowResized w h ->
            ( { model | resolution = { width = toFloat w, height = toFloat h } }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )




-- VIEW


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
            viewHome model

        Movies ->
            viewGridScreen "Movies" model.filter model.search (Catalog.movies Catalog.catalog) yearMeta

        Shows ->
            viewGridScreen "Shows" model.filter model.search (Catalog.shows Catalog.catalog) durationMeta

        Library ->
            viewGridScreen "LIBRARY" model.filter model.search Catalog.catalog yearMeta

        Detail ->
            viewDetail (Catalog.find model.activeId)

        Player ->
            viewPlayer (Catalog.find model.activeId)

        Requests ->
            viewRequests model



-- HEADER


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



-- HOME


viewHome : Model -> Html Msg
viewHome model =
    let
        active =
            Catalog.find model.activeId

        withRow slice =
            slice Catalog.catalog
    in
    div []
        [ viewHero active
        , div [ class "ph4 pt4 pb5 flex flex-column gap-7" ]
            [ viewRow "Continue Watching" "row-continue" continueCard (List.take 9 Catalog.catalog)
            , viewRow "Trending Now" "row-trending" rowCard (withRow (List.drop 3 >> List.take 10))
            , viewRow "Recently Added" "row-recent" rowCard (withRow (List.drop 6 >> List.take 10))
            ]
        ]


viewHero : Title -> Html Msg
viewHero active =
    div
        [ class "relative hero-bg bb b--default flex items-end pa-7"
        , style "height" "440px"
        ]
        [ div [ class "flex flex-column gap-3", style "max-width" "640px" ]
            [ div [ class "f1 ttu b tracked-wide fg-primary" ] [ text active.title ]
            , div [ class "flex items-center gap-3 f6 fg-secondary" ]
                [ span [] [ text (String.fromInt active.year) ]
                , span [] [ text "·" ]
                , span [] [ text active.duration ]
                , span [] [ text "·" ]
                , span
                    [ class "ba b--default ttu tracked-wide"
                    , style "padding" "2px 8px"
                    ]
                    [ text (Catalog.genreLabel active.genre) ]
                ]
            , div [ class "f5 fg-secondary lh-copy" ] [ text active.synopsis ]
            , div [ class "flex gap-4 mt3" ]
                [ button [ class "btn btn-primary", onClick OpenPlayer ] [ text "▶ Play" ]
                , button [ class "btn btn-ghost", onClick (OpenDetail active.id) ] [ text "More Info" ]
                ]
            ]
        ]


viewRow : String -> String -> (Title -> Html Msg) -> List Title -> Html Msg
viewRow label rowId card titles =
    div []
        [ div [ class "flex items-center justify-between mb3" ]
            [ div [ class "f6 tracked-wider ttu fg-secondary" ] [ text label ]
            , div [ class "flex gap-2" ]
                [ div [ class "arrow-btn", onClick (ScrollRow rowId -520) ] [ text "‹" ]
                , div [ class "arrow-btn", onClick (ScrollRow rowId 520) ] [ text "›" ]
                ]
            ]
        , div [ id rowId, class "row-scroll flex gap-4 pb2" ] (List.map card titles)
        ]



-- POSTER CARDS


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


{-| 160x240 row card: year top-right and title fade in on hover. -}
rowCard : Title -> Html Msg
rowCard title =
    div
        [ class "poster relative pointer flex-none"
        , style "width" "160px"
        , style "height" "240px"
        , onClick (OpenDetail title.id)
        ]
        [ monogram "64px" title
        , div [ class "reveal absolute absolute--fill flex flex-column justify-between" ]
            [ div [ class "pa2 tr f7 fg-muted" ] [ text (String.fromInt title.year) ]
            , cardTitle title
            ]
        ]


{-| Continue Watching card: hover title plus an always-visible progress bar. -}
continueCard : Title -> Html Msg
continueCard title =
    div
        [ class "poster relative pointer flex-none"
        , style "width" "160px"
        , style "height" "240px"
        , onClick (OpenDetail title.id)
        ]
        [ monogram "64px" title
        , div [ class "reveal absolute absolute--fill flex items-end" ] [ cardTitle title ]
        , div
            [ class "absolute left-0 bottom-0"
            , style "height" "3px"
            , style "width" (String.fromInt title.progress ++ "%")
            , style "background" "var(--gray-5)"
            ]
            []
        ]


{-| Detail's related row: hover reveals title only. -}
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



-- MOVIES / SHOWS / LIBRARY


yearMeta : Title -> String
yearMeta title =
    String.fromInt title.year


durationMeta : Title -> String
durationMeta =
    .duration


viewGridScreen : String -> Maybe Genre -> String -> List Title -> (Title -> String) -> Html Msg
viewGridScreen pageTitle filter search titles meta =
    let
        results =
            titles |> Catalog.byGenre filter |> Catalog.searchBy search
    in
    div [ class "pa4" ]
        [ div [ class "text-xl ttu tracked-wide mb-5 fg-primary" ] [ text pageTitle ]
        , viewFilterChips filter
        , div [ class "grid-fill" ]
            (List.map (gridCard meta) results)
        ]


viewFilterChips : Maybe Genre -> Html Msg
viewFilterChips active =
    let
        chip ( label, filter ) =
            div
                [ class "pointer ba f7 tracked-wide ttu"
                , class
                    (if active == filter then
                        "b--strong fg-primary"

                     else
                        "b--default fg-secondary"
                    )
                , style "padding" "6px 16px"
                , onClick (SetFilter filter)
                ]
                [ text label ]
    in
    div [ class "flex flex-wrap gap-3 mb4" ]
        (List.map chip
            [ ( "ALL", Nothing )
            , ( "ACTION", Just Action )
            , ( "SCIFI", Just Scifi )
            , ( "DRAMA", Just Drama )
            , ( "THRILLER", Just Thriller )
            ]
        )


gridCard : (Title -> String) -> Title -> Html Msg
gridCard meta title =
    div
        [ class "poster relative pointer"
        , style "height" "240px"
        , onClick (OpenDetail title.id)
        ]
        [ monogram "64px" title
        , div [ class "reveal absolute absolute--fill flex flex-column justify-between" ]
            [ div [ class "pa2 tr f7 fg-muted" ] [ text (meta title) ]
            , cardTitle title
            ]
        ]



-- DETAIL


viewDetail : Title -> Html Msg
viewDetail active =
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



-- PLAYER


viewPlayer : Title -> Html Msg
viewPlayer active =
    div [ class "min-vh-100 flex flex-column" ]
        [ div [ class "flex items-center gap-4 pv3 ph4" ]
            [ div [ class "pointer f6 fg-secondary", onClick BackFromPlayer ] [ text "← BACK" ]
            , div [ class "f6 tracked-wide ttu" ] [ text active.title ]
            ]
        , div [ class "flex-auto flex items-center justify-center ph-7" ]
            [ div
                [ class "w-100 stage-bg ba b--default relative flex items-center justify-center"
                , style "max-width" "1100px"
                , style "aspect-ratio" "16/9"
                ]
                [ div
                    [ style "width" "0"
                    , style "height" "0"
                    , style "border-top" "24px solid transparent"
                    , style "border-bottom" "24px solid transparent"
                    , style "border-left" "36px solid var(--white)"
                    , style "opacity" ".85"
                    ]
                    []
                ]
            ]
        , div [ class "ph-7 pv4 flex flex-column gap-3" ]
            [ div [ class "relative w-100", style "height" "3px", style "background" "var(--gray-2)" ]
                [ div
                    [ class "absolute left-0 top-0 h-100"
                    , style "width" "42%"
                    , style "background" "var(--white)"
                    ]
                    []
                ]
            , div [ class "flex items-center justify-between" ]
                [ div [ class "flex items-center gap-5 text-md" ]
                    [ span [ class "pointer" ] [ text "◄◄" ]
                    , span [ class "pointer" ] [ text "❚❚" ]
                    , span [ class "pointer" ] [ text "►►" ]
                    , span [ class "f7 fg-muted tracked-wide" ] [ text "00:52:14 / 02:04:00" ]
                    ]
                , div [ class "flex items-center gap-5 f6 fg-secondary" ]
                    [ span [ class "pointer" ] [ text "VOL" ]
                    , span [ class "pointer" ] [ text "CC" ]
                    , span [ class "pointer" ] [ text "⛶" ]
                    ]
                ]
            ]
        ]



-- REQUESTS


viewRequests : Model -> Html Msg
viewRequests model =
    div [ class "pa4" ]
        [ div [ class "flex items-center justify-between mb4" ]
            [ div [ class "text-xl ttu tracked-wide fg-primary" ] [ text "Requests" ]
            , button [ class "btn btn-primary", onClick (SetTab Discover) ] [ text "+ New Request" ]
            ]
        , div [ class "flex gap-5 bb b--default mb4" ]
            [ requestTab model.requestsTab "My Requests" Mine
            , requestTab model.requestsTab "Discover" Discover
            ]
        , case model.requestsTab of
            Mine ->
                viewMyRequests model.requests

            Discover ->
                viewDiscover model.requests
        ]


requestTab : Tab -> String -> Tab -> Html Msg
requestTab current label target =
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



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions =
            \_ ->
                Sub.batch
                    [ Browser.Events.onAnimationFrameDelta Tick
                    , Browser.Events.onResize WindowResized
                    ]
        }
