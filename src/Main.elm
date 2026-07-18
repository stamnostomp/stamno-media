port module Main exposing (main)

import Api
import Browser
import Browser.Dom as Dom
import Browser.Events
import Catalog exposing (Genre(..), MediaType(..), Title)
import Dict exposing (Dict)
import Html exposing (Html, button, div, input, span, text)
import Html.Attributes exposing (autoplay, class, height, id, src, style, width)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import Math.Vector2 as Vec2
import Request
import Shaders.Background
import Shaders.Mesh
import Task
import WebGL



-- PORTS


port scrollRow : { rowId : String, delta : Float } -> Cmd msg


{-| Commands sent down to the JS side to drive the real `<video>` element
(Elm has no built-in media-element bindings). `action` is one of "play",
"pause", "seek", "skip", "mute", "volume", "fullscreen"; `value` carries
the seek target / skip delta / volume level and is ignored otherwise.
-}
port videoCommand : { action : String, value : Float } -> Cmd msg


{-| Status pushed up from the JS side on every timeupdate/play/pause/
volumechange event of the real `<video>` element. -}
port videoStatus : (VideoState -> msg) -> Sub msg



-- MODEL


type Screen
    = Home
    | Movies
    | Shows
    | Library
    | Detail
    | RealDetail
    | Player
    | Requests


type Tab
    = Mine
    | Discover


{-| Which source the Player screen is showing: the styled demo player (no
real file behind it) or a real file streamed from the media server.
-}
type NowPlaying
    = DemoPlaying
    | RealPlaying Api.Item


{-| Mirrors the real `<video>` element's state, kept in sync via the
'videoStatus' port. -}
type alias VideoState =
    { currentTime : Float
    , duration : Float
    , paused : Bool
    , muted : Bool
    , volume : Float
    }


{-| Assume paused until a real 'videoStatus' event says otherwise: most
browsers block unmuted `autoplay`, so starting from `paused = False` here
would desync the play/pause button and icon from the actual (likely still
paused) video until the first status event arrives. -}
initialVideoState : VideoState
initialVideoState =
    { currentTime = 0, duration = 0, paused = True, muted = False, volume = 1 }


type alias Model =
    { screen : Screen
    , activeId : Int
    , filter : Maybe Genre
    , search : String
    , requestsTab : Tab
    , requests : Dict Int Request.Entry
    , time : Float
    , resolution : { width : Float, height : Float }
    , library : List Api.Item
    , libraryError : Maybe String
    , continueWatching : List Api.ContinueItem
    , popular : List Api.Item
    , newItems : List Api.Item
    , nowPlaying : NowPlaying
    , videoState : VideoState
    , controlsVisible : Bool
    , idleTime : Float
    , resumeProgress : Maybe Int
    , previousScreen : Screen
    , activeRealItem : Maybe Api.Item
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
      , library = []
      , libraryError = Nothing
      , continueWatching = []
      , popular = []
      , newItems = []
      , nowPlaying = DemoPlaying
      , videoState = initialVideoState
      , controlsVisible = True
      , idleTime = 0
      , resumeProgress = Nothing
      , previousScreen = Home
      , activeRealItem = Nothing
      }
    , Cmd.batch
        [ Task.perform GotViewport Dom.getViewport
        , Api.fetchLibrary GotLibrary
        , Api.fetchContinueWatching GotContinueWatching
        , Api.fetchPopular GotPopular
        , Api.fetchNew GotNew
        ]
    )



-- UPDATE


type Msg
    = NavigateTo Screen
    | OpenDetail Int
    | OpenPlayer
    | OpenRealDetail Api.Item
    | OpenRealPlayer Api.Item
    | BackFromPlayer
    | SetFilter (Maybe Genre)
    | SetSearch String
    | SetTab Tab
    | RequestTitle Int
    | ScrollRow String Float
    | Tick Float
    | GotViewport Dom.Viewport
    | WindowResized Int Int
    | GotLibrary (Result Http.Error (List Api.Item))
    | GotContinueWatching (Result Http.Error (List Api.ContinueItem))
    | GotPopular (Result Http.Error (List Api.Item))
    | GotNew (Result Http.Error (List Api.Item))
    | VideoTogglePlay
    | VideoSeekTo Float
    | VideoSkip Float
    | VideoToggleMute
    | VideoSetVolume Float
    | VideoWheelVolume Float
    | VideoFullscreen
    | GotVideoStatus VideoState
    | PlayerActivity
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NavigateTo screen ->
            ( { model | screen = screen }, Cmd.none )

        OpenDetail titleId ->
            ( { model | screen = Detail, activeId = titleId }, Cmd.none )

        OpenPlayer ->
            ( { model | screen = Player, nowPlaying = DemoPlaying, previousScreen = model.screen }
            , Cmd.none
            )

        OpenRealDetail item ->
            ( { model | screen = RealDetail, activeRealItem = Just item, previousScreen = model.screen }
            , Cmd.none
            )

        OpenRealPlayer item ->
            ( { model
                | screen = Player
                , nowPlaying = RealPlaying item
                , videoState = initialVideoState
                , controlsVisible = True
                , idleTime = 0
                , previousScreen = model.screen
                , resumeProgress =
                    model.continueWatching
                        |> List.filter (\c -> c.id == item.id)
                        |> List.head
                        |> Maybe.map .progress
              }
            , Cmd.none
            )

        BackFromPlayer ->
            case model.nowPlaying of
                RealPlaying item ->
                    ( { model
                        | screen = model.previousScreen
                        , continueWatching =
                            upsertContinueWatching item model.videoState model.continueWatching
                      }
                    , videoCommand { action = "pause", value = 0 }
                    )

                DemoPlaying ->
                    ( { model | screen = model.previousScreen }, Cmd.none )

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
            let
                isPlayingVideo =
                    model.screen == Player && not model.videoState.paused

                newIdleTime =
                    if isPlayingVideo then
                        model.idleTime + delta

                    else
                        0

                newControlsVisible =
                    if not isPlayingVideo then
                        True

                    else if newIdleTime > idleHideThreshold then
                        False

                    else
                        model.controlsVisible
            in
            ( { model
                | time = model.time + delta * 0.001
                , idleTime = newIdleTime
                , controlsVisible = newControlsVisible
              }
            , Cmd.none
            )

        GotViewport vp ->
            ( { model | resolution = { width = vp.viewport.width, height = vp.viewport.height } }
            , Cmd.none
            )

        WindowResized w h ->
            ( { model | resolution = { width = toFloat w, height = toFloat h } }, Cmd.none )

        GotLibrary result ->
            case result of
                Ok items ->
                    ( { model | library = items, libraryError = Nothing }, Cmd.none )

                Err err ->
                    ( { model | libraryError = Just (httpErrorToString err) }, Cmd.none )

        GotContinueWatching result ->
            ( { model | continueWatching = Result.withDefault model.continueWatching result }
            , Cmd.none
            )

        GotPopular result ->
            ( { model | popular = Result.withDefault model.popular result }, Cmd.none )

        GotNew result ->
            ( { model | newItems = Result.withDefault model.newItems result }, Cmd.none )

        VideoTogglePlay ->
            ( model
            , videoCommand
                { action =
                    if model.videoState.paused then
                        "play"

                    else
                        "pause"
                , value = 0
                }
            )

        VideoSeekTo fraction ->
            ( model, videoCommand { action = "seek", value = fraction * model.videoState.duration } )

        VideoSkip delta ->
            ( model, videoCommand { action = "skip", value = delta } )

        VideoToggleMute ->
            ( model, videoCommand { action = "mute", value = 0 } )

        VideoSetVolume vol ->
            ( model, videoCommand { action = "volume", value = clamp 0 1 vol } )

        VideoWheelVolume delta ->
            ( model
            , videoCommand { action = "volume", value = clamp 0 1 (model.videoState.volume + delta) }
            )

        VideoFullscreen ->
            ( model, videoCommand { action = "fullscreen", value = 0 } )

        GotVideoStatus status ->
            ( { model | videoState = status }, Cmd.none )

        PlayerActivity ->
            ( { model | idleTime = 0, controlsVisible = True }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


{-| Milliseconds of no mouse activity, while playing, before the player
controls fade out. Matches "same behaviour" in and out of real fullscreen,
since fullscreening the whole stage (see 'viewRealPlayer') keeps this Elm
state and its view driving the overlay either way. -}
idleHideThreshold : Float
idleHideThreshold =
    3000


{-| Updates the local continue-watching list from the video's own current
state, instead of waiting on a round trip to the server. Without this, a
fresh seek right before leaving the player wouldn't be reflected until
the next `/continuewatching` fetch (which doesn't happen again on its
own), so re-opening the same item would silently resume from the old,
stale position. -}
upsertContinueWatching : Api.Item -> VideoState -> List Api.ContinueItem -> List Api.ContinueItem
upsertContinueWatching item videoState existing =
    let
        withoutItem =
            List.filter (\c -> c.id /= item.id) existing
    in
    if videoState.duration <= 0 then
        existing

    else
        let
            percent =
                round (100 * videoState.currentTime / videoState.duration)
        in
        if percent > 2 && percent < 95 then
            { id = item.id, title = item.title, size = item.size, mime = item.mime, progress = percent }
                :: withoutItem

        else
            withoutItem


httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.BadUrl url ->
            "bad url: " ++ url

        Http.Timeout ->
            "request timed out"

        Http.NetworkError ->
            "network error (is the media server running?)"

        Http.BadStatus code ->
            "server returned " ++ String.fromInt code

        Http.BadBody body ->
            "unexpected response: " ++ body




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
            viewLibraryScreen "MOVIES" model

        Shows ->
            viewGridScreen "Shows" model.filter model.search (Catalog.shows Catalog.catalog) durationMeta

        Library ->
            viewLibraryScreen "LIBRARY" model

        Detail ->
            viewDetail (Catalog.find model.activeId)

        RealDetail ->
            viewRealDetail model

        Player ->
            viewPlayer model

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
    div []
        [ viewHero (Catalog.find model.activeId)
        , div [ class "ph4 pt4 pb5 flex flex-column gap-7" ]
            [ viewMaybeRow "Continue Watching" "row-continue" continueWatchingCard model.continueWatching
            , viewMaybeRow "Trending Now" "row-trending" libraryCard model.popular
            , viewMaybeRow "Recently Added" "row-recent" libraryCard model.newItems
            ]
        ]


{-| Omits the row entirely (rather than showing an empty scroller) when
the server has nothing for it yet, e.g. "Continue Watching" before
anything has been played. -}
viewMaybeRow : String -> String -> (a -> Html Msg) -> List a -> Html Msg
viewMaybeRow label rowId card items =
    if List.isEmpty items then
        text ""

    else
        viewRow label rowId card items


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


viewRow : String -> String -> (a -> Html Msg) -> List a -> Html Msg
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


{-| Continue Watching card, real-data version: hover title plus an
always-visible progress bar reflecting the server-reported watch percent. -}
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


durationMeta : Title -> String
durationMeta =
    .duration


{-| The real, API-backed library: whatever functonal-media-server finds
under its configured media path. No genre/year/synopsis metadata exists
for these files, so this is a plainer grid than the demo catalog's, and
clicking a card opens its detail page rather than streaming directly.
-}
viewLibraryScreen : String -> Model -> Html Msg
viewLibraryScreen pageTitle model =
    let
        results =
            librarySearch model.search model.library
    in
    div [ class "pa4" ]
        [ div [ class "text-xl ttu tracked-wide mb-5 fg-primary" ] [ text pageTitle ]
        , case model.libraryError of
            Just err ->
                div [ class "f6 fg-muted mb4" ] [ text ("Could not reach media server: " ++ err) ]

            Nothing ->
                text ""
        , if List.isEmpty results then
            div [ class "f6 fg-muted" ]
                [ text
                    (if model.libraryError /= Nothing then
                        ""

                     else
                        "No media found."
                    )
                ]

          else
            div [ class "grid-fill" ] (List.map libraryCard results)
        ]


librarySearch : String -> List Api.Item -> List Api.Item
librarySearch query items =
    if String.isEmpty query then
        items

    else
        let
            lower =
                String.toLower query
        in
        List.filter (\item -> String.contains lower (String.toLower item.title)) items


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


{-| Fixed-width variant of 'libraryCard' for a horizontal row-scroll
(e.g. "More From Your Library"), same as how 'relatedCard' differs from
the grid cards for the demo catalog. -}
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


{-| Detail page for a real, API-backed item, matching the demo catalog's
`viewDetail` layout (backdrop banner, overlapping poster/info panel,
description line, "More From Your Library" row) with fields that actually
exist for a real file substituted for genre/cast/synopsis. -}
viewRealDetail : Model -> Html Msg
viewRealDetail model =
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
                        , case maybeProgress of
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

                            Nothing ->
                                text ""
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



-- PLAYER


viewPlayer : Model -> Html Msg
viewPlayer model =
    case model.nowPlaying of
        RealPlaying item ->
            viewRealPlayer item model.videoState model.controlsVisible model.resumeProgress

        DemoPlaying ->
            viewDemoPlayer (Catalog.find model.activeId)


{-| Actual playback of a file streamed from the media server, via a
`<video>` element hitting `GET /stream/:id` (byte-range requests and all).
The video fills the whole viewport; the back link and control bar are
absolutely-positioned overlays on top of it, each with a gradient behind
them for legibility. Native controls are disabled; play/pause/seek/mute/
fullscreen are driven through the 'videoCommand' port. The play/pause
toggle lives on the `<video>` element itself (not an ancestor), so clicks
on the header/control overlays don't also toggle playback.

The whole stage (not just the `<video>`) is what goes fullscreen — see the
'fullscreen' case in index.html's port glue — since the Fullscreen API only
renders the fullscreened element's own descendants; fullscreening the video
alone would drop these overlays entirely. Because show/hide of the overlays
is driven by this same 'controlsVisible' state either way, behaviour is
identical windowed or fullscreen.

If 'maybeResumeProgress' is set (the item appears in continue-watching),
the video carries a `data-resume-percent` attribute that index.html's JS
reads once, on `loadedmetadata`, to seek to that position before playback
is visible to the user.
-}
viewRealPlayer : Api.Item -> VideoState -> Bool -> Maybe Int -> Html Msg
viewRealPlayer item videoState controlsVisible maybeResumeProgress =
    div
        [ id "player-stage"
        , class "fixed top-0 left-0 w-100 vh-100 overflow-hidden"
        , style "background" "black"
        , Html.Events.on "mousemove" (Decode.succeed PlayerActivity)
        ]
        [ Html.node "video"
            [ id "media-player"
            , Html.Attributes.attribute "data-item-id" item.id
            , Html.Attributes.attribute "data-resume-percent"
                (maybeResumeProgress |> Maybe.map String.fromInt |> Maybe.withDefault "")
            , autoplay True
            , src (Api.streamUrl item.id)
            , onClick VideoTogglePlay
            , style "position" "absolute"
            , style "top" "0"
            , style "left" "0"
            , style "width" "100%"
            , style "height" "100%"
            , style "object-fit" "contain"
            ]
            []
        , if videoState.paused then
            div
                [ style "position" "absolute"
                , style "top" "50%"
                , style "left" "50%"
                , style "transform" "translate(-50%, -50%)"
                , style "width" "0"
                , style "height" "0"
                , style "border-top" "24px solid transparent"
                , style "border-bottom" "24px solid transparent"
                , style "border-left" "36px solid var(--white)"
                , style "opacity" ".85"
                , style "pointer-events" "none"
                ]
                []

          else
            text ""
        , div
            [ class "absolute top-0 left-0 w-100 flex items-center justify-between pv3 ph4"
            , style "background" "linear-gradient(180deg, rgba(0,0,0,0.75), transparent)"
            , style "opacity"
                (if controlsVisible then
                    "1"

                 else
                    "0"
                )
            , style "pointer-events"
                (if controlsVisible then
                    "auto"

                 else
                    "none"
                )
            , style "transition" "opacity 0.3s"
            ]
            [ div [ class "f6 tracked-wide ttu" ] [ text item.title ]
            , div [ class "pointer f6 fg-secondary", onClick BackFromPlayer ] [ text "← BACK" ]
            ]
        , div
            [ class "absolute bottom-0 left-0 w-100 ph-7 pv4 flex flex-column gap-3 fade-up-strong"
            , style "opacity"
                (if controlsVisible then
                    "1"

                 else
                    "0"
                )
            , style "pointer-events"
                (if controlsVisible then
                    "auto"

                 else
                    "none"
                )
            , style "transition" "opacity 0.3s"
            ]
            [ videoSeekBar videoState
            , div [ class "flex items-center justify-between" ]
                [ div [ class "flex items-center gap-5 text-md" ]
                    [ span [ class "pointer", onClick (VideoSkip -10) ] [ text "◄◄" ]
                    , span
                        [ class "pointer tc"
                        , style "display" "inline-block"
                        , style "width" "1.25em"
                        , style "font-size" "1.75rem"
                        , onClick VideoTogglePlay
                        ]
                        [ text
                            (if videoState.paused then
                                "▶"

                             else
                                "❚❚"
                            )
                        ]
                    , span [ class "pointer", onClick (VideoSkip 10) ] [ text "►►" ]
                    , span [ class "f7 fg-muted tracked-wide" ]
                        [ text (formatTime videoState.currentTime ++ " / " ++ formatTime videoState.duration) ]
                    ]
                , div [ class "flex items-center gap-5 f6 fg-secondary" ]
                    [ volControl videoState
                    , span [ class "pointer" ] [ text "CC" ]
                    , span [ class "pointer", onClick VideoFullscreen ] [ text "⛶" ]
                    ]
                ]
            ]
        ]


{-| Clickable/scrubbable progress bar: click position (as a fraction of the
bar's width) maps directly to a seek target. -}
videoSeekBar : VideoState -> Html Msg
videoSeekBar videoState =
    let
        pct =
            if videoState.duration > 0 then
                (videoState.currentTime / videoState.duration) * 100

            else
                0
    in
    div
        [ class "relative w-100 pointer"
        , style "height" "3px"
        , style "background" "var(--gray-2)"
        , onClickFraction VideoSeekTo
        ]
        [ div
            [ class "absolute left-0 top-0 h-100"
            , style "width" (String.fromFloat pct ++ "%")
            , style "background" "var(--white)"
            , style "pointer-events" "none"
            ]
            []
        ]


{-| The VOL/MUTED label plus a slider that pops up above it on hover
(`.vol-control`/`.vol-slider` in stamno.css). The slider is a native range
input, so click-to-set and drag both work for free; a wheel listener on
the whole control adjusts volume in small steps without needing the
popup open first. -}
volControl : VideoState -> Html Msg
volControl videoState =
    div
        [ class "vol-control relative pointer"
        , Html.Events.preventDefaultOn "wheel" wheelVolumeDecoder
        ]
        [ span [ onClick VideoToggleMute ]
            [ text
                (if videoState.muted then
                    "MUTED"

                 else
                    "VOL"
                )
            ]
        , div [ class "vol-slider absolute" ]
            [ input
                [ Html.Attributes.type_ "range"
                , Html.Attributes.min "0"
                , Html.Attributes.max "1"
                , Html.Attributes.step "0.01"
                , Html.Attributes.value
                    (String.fromFloat
                        (if videoState.muted then
                            0

                         else
                            videoState.volume
                        )
                    )
                , onRangeInput VideoSetVolume
                ]
                []
            ]
        ]


wheelVolumeDecoder : Decode.Decoder ( Msg, Bool )
wheelVolumeDecoder =
    Decode.field "deltaY" Decode.float
        |> Decode.map
            (\dy ->
                ( VideoWheelVolume
                    (if dy > 0 then
                        -0.05

                     else
                        0.05
                    )
                , True
                )
            )


onRangeInput : (Float -> Msg) -> Html.Attribute Msg
onRangeInput toMsg =
    Html.Events.on "input"
        (Html.Events.targetValue
            |> Decode.andThen
                (\str ->
                    case String.toFloat str of
                        Just v ->
                            Decode.succeed (toMsg v)

                        Nothing ->
                            Decode.fail "not a float"
                )
        )


{-| Maps a click's horizontal position within the element to a 0..1
fraction of the element's width, e.g. for scrubbing a progress bar.

The fill overlay inside the bar has `pointer-events: none` so that
`event.target` here is always the bar itself, never the overlay — otherwise
a click landing on the (already-played) filled portion would compute its
offset and width against the overlay's own box instead of the full bar.
-}
onClickFraction : (Float -> Msg) -> Html.Attribute Msg
onClickFraction toMsg =
    Html.Events.on "click"
        (Decode.map2 (\x w -> toMsg (clamp 0 1 (x / w)))
            (Decode.field "offsetX" Decode.float)
            (Decode.at [ "target", "clientWidth" ] Decode.float)
        )


formatTime : Float -> String
formatTime seconds =
    if isNaN seconds || isInfinite seconds || seconds < 0 then
        "00:00:00"

    else
        let
            total =
                floor seconds

            h =
                total // 3600

            m =
                modBy 60 (total // 60)

            s =
                modBy 60 total

            pad n =
                String.padLeft 2 '0' (String.fromInt n)
        in
        pad h ++ ":" ++ pad m ++ ":" ++ pad s


viewDemoPlayer : Title -> Html Msg
viewDemoPlayer active =
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
                    , videoStatus GotVideoStatus
                    ]
        }
