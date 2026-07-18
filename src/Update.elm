module Update exposing (update)

import Api
import Http
import Ports
import Request
import Types
    exposing
        ( Model
        , Msg(..)
        , NowPlaying(..)
        , Screen(..)
        , VideoState
        , initialVideoState
        )


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
                    , Ports.videoCommand { action = "pause", value = 0 }
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
            ( model, Ports.scrollRow { rowId = rowId, delta = delta } )

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
            , Ports.videoCommand
                { action =
                    if model.videoState.paused then
                        "play"

                    else
                        "pause"
                , value = 0
                }
            )

        VideoSeekTo fraction ->
            ( model
            , Ports.videoCommand { action = "seek", value = fraction * model.videoState.duration }
            )

        VideoSkip delta ->
            ( model, Ports.videoCommand { action = "skip", value = delta } )

        VideoToggleMute ->
            ( model, Ports.videoCommand { action = "mute", value = 0 } )

        VideoSetVolume vol ->
            ( model, Ports.videoCommand { action = "volume", value = clamp 0 1 vol } )

        VideoWheelVolume delta ->
            ( model
            , Ports.videoCommand
                { action = "volume", value = clamp 0 1 (model.videoState.volume + delta) }
            )

        VideoFullscreen ->
            ( model, Ports.videoCommand { action = "fullscreen", value = 0 } )

        GotVideoStatus status ->
            ( { model | videoState = status }, Cmd.none )

        PlayerActivity ->
            ( { model | idleTime = 0, controlsVisible = True }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


{-| Milliseconds of no mouse activity, while playing, before the player
controls fade out.
-}
idleHideThreshold : Float
idleHideThreshold =
    3000


{-| Updates the local continue-watching list from the video's own current
state, instead of waiting on a round trip to the server. Without this, a
fresh seek right before leaving the player wouldn't be reflected until
the next `/continuewatching` fetch (which doesn't happen again on its
own), so re-opening the same item would silently resume from the old,
stale position.
-}
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
