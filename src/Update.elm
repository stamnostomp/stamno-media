module Update exposing (update)

import Api
import Http
import Ports
import Types
    exposing
        ( Model
        , Msg(..)
        , NowPlaying(..)
        , Screen(..)
        , Tab(..)
        , VideoState
        , initialVideoState
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NavigateTo screen ->
            ( { model | screen = screen, userMenuOpen = False }
            , if screen == Requests && List.isEmpty model.myRequests then
                fetchRequestsData model.token

              else
                Cmd.none
            )

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

        ToggleUserMenu ->
            ( { model | userMenuOpen = not model.userMenuOpen }, Cmd.none )

        CloseUserMenu ->
            ( { model | userMenuOpen = False }, Cmd.none )

        SetFilter filter ->
            ( { model | filter = filter }, Cmd.none )

        SetSearch query ->
            ( { model | search = query }, Cmd.none )

        SetTab tab ->
            case tab of
                Mine ->
                    ( { model | requestsTab = tab }, Api.fetchRequests model.token GotRequests )

                Discover ->
                    ( { model | requestsTab = tab }
                    , if List.isEmpty model.trackers then
                        Api.fetchTrackers model.token GotTrackers

                      else
                        Cmd.none
                    )

        GotRequests result ->
            case result of
                Ok rs ->
                    ( { model | myRequests = rs, needsAuth = False }, Cmd.none )

                Err err ->
                    if isUnauthorized err then
                        ( { model | needsAuth = True }, Cmd.none )

                    else
                        ( model, Cmd.none )

        GotTrackers result ->
            case result of
                Ok ts ->
                    let
                        active =
                            case model.activeTracker of
                                Just _ ->
                                    model.activeTracker

                                Nothing ->
                                    ts |> List.filter .enabled |> List.head |> Maybe.map .id
                    in
                    ( { model | trackers = ts, activeTracker = active, needsAuth = False }, Cmd.none )

                Err err ->
                    if isUnauthorized err then
                        ( { model | needsAuth = True }, Cmd.none )

                    else
                        ( model, Cmd.none )

        SetReqQuery q ->
            ( { model | reqQuery = q }, Cmd.none )

        SetActiveTracker tid ->
            -- m26: the empty value (the ALL TRACKERS option) maps to Nothing
            ( { model | activeTracker = if tid == "" then Nothing else Just tid }, Cmd.none )

        SearchReleases ->
            ( { model | reqLoading = True, reqNotice = Nothing }, searchIndexersCmd model )

        PickPopular item ->
            -- m26: picking a popular card fills the query and searches all
            -- enabled indexers (or the selected one) for it
            let
                model2 =
                    { model | reqQuery = item.name, reqLoading = True, reqNotice = Nothing }
            in
            ( model2, searchIndexersCmd model2 )

        GotDiscoverPopular result ->
            case result of
                Ok ( movies, shows ) ->
                    ( { model | popularMovies = movies, popularShows = shows, needsAuth = False }, Cmd.none )

                Err err ->
                    if isUnauthorized err then
                        ( { model | needsAuth = True }, Cmd.none )

                    else
                        ( model, Cmd.none )

        GotReleases result ->
            case result of
                Ok rs ->
                    ( { model | releases = rs, reqLoading = False }, Cmd.none )

                Err _ ->
                    ( { model | releases = [], reqLoading = False, reqNotice = Just "SEARCH FAILED" }, Cmd.none )

        CreateRelease rel ->
            let
                payload =
                    { title =
                        if String.isEmpty model.reqQuery then
                            rel.title

                        else
                            model.reqQuery
                    , year = Nothing
                    , mediaType = Just "movie"
                    , trackerId =
                        -- m28: prefer the release's own IndexerId (works when
                        -- ALL TRACKERS is selected); fall back to the picker
                        if String.isEmpty rel.trackerId then
                            model.activeTracker

                        else
                            Just rel.trackerId
                    , release = Just rel.title
                    , sizeBytes = rel.sizeBytes
                    , magnet = rel.magnet
                    , downloadUrl = rel.downloadUrl
                    }
            in
            ( { model | reqLoading = True, reqNotice = Nothing }
            , Api.createRequest model.token payload GotCreateRequest
            )

        GotCreateRequest code result ->
            case result of
                Ok _ ->
                    ( { model | reqNotice = Just "REQUESTED", reqLoading = False, requestsTab = Mine }
                    , Api.fetchRequests model.token GotRequests
                    )

                Err _ ->
                    let
                        notice =
                            if code == 409 then
                                "ALREADY IN PROGRESS"

                            else
                                "REQUEST FAILED"
                    in
                    ( { model | reqNotice = Just notice, reqLoading = False }
                    , Api.fetchRequests model.token GotRequests
                    )

        ClearReqNotice ->
            ( { model | reqNotice = Nothing }, Cmd.none )

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
                    ( { model | library = items, libraryError = Nothing, needsAuth = False }, Cmd.none )

                Err err ->
                    if isUnauthorized err then
                        ( { model | needsAuth = True }, Cmd.none )

                    else
                        ( { model | libraryError = Just (httpErrorToString err) }, Cmd.none )

        GotContinueWatching result ->
            case result of
                Ok items ->
                    ( { model | continueWatching = items, needsAuth = False }, Cmd.none )

                Err err ->
                    if isUnauthorized err then
                        ( { model | needsAuth = True }, Cmd.none )

                    else
                        ( model, Cmd.none )

        GotPopular result ->
            case result of
                Ok items ->
                    ( { model | popular = items, needsAuth = False }, Cmd.none )

                Err err ->
                    if isUnauthorized err then
                        ( { model | needsAuth = True }, Cmd.none )

                    else
                        ( model, Cmd.none )

        GotNew result ->
            case result of
                Ok items ->
                    ( { model | newItems = items, needsAuth = False }, Cmd.none )

                Err err ->
                    if isUnauthorized err then
                        ( { model | needsAuth = True }, Cmd.none )

                    else
                        ( model, Cmd.none )

        GotStoredToken stored ->
            if String.isEmpty stored then
                ( model, Cmd.none )

            else
                ( { model | token = Just stored }, fetchAll (Just stored) )

        SetLoginUser v ->
            ( { model | loginUser = v }, Cmd.none )

        SetLoginPass v ->
            ( { model | loginPass = v }, Cmd.none )

        SubmitLogin ->
            ( { model | loginError = Nothing }, Api.fetchAuth model.loginUser model.loginPass GotAuth )

        GotAuth result ->
            case result of
                Ok r ->
                    ( { model
                        | token = Just r.token
                        , authUser = Just { name = r.userName, isAdmin = r.isAdmin }
                        , needsAuth = False
                        , loginError = Nothing
                        , loginPass = ""
                      }
                    , Cmd.batch [ Ports.storeToken r.token, fetchAll (Just r.token) ]
                    )

                Err err ->
                    ( { model | loginError = Just (if isUnauthorized err then "INVALID CREDENTIALS" else "SERVER UNREACHABLE") }
                    , Cmd.none
                    )

        SignOut ->
            ( { model
                | token = Nothing
                , authUser = Nothing
                , needsAuth = True
                , loginError = Nothing
                , library = []
                , continueWatching = []
                , popular = []
                , newItems = []
                , myRequests = []
                , trackers = []
                , releases = []
                , activeTracker = Nothing
                , reqNotice = Nothing
                , reqLoading = False
              }
            , Ports.storeToken ""
            )

        GotUnauthorized ->
            ( { model | needsAuth = True }, Cmd.none )

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


{-| True when the server rejected the request for a missing/invalid token
(status 401): the UI then switches to the login screen. -}
isUnauthorized : Http.Error -> Bool
isUnauthorized err =
    case err of
        Http.BadStatus code ->
            code == 401

        _ ->
            False


{-| Re-fetch every data feed with the given token. -}
fetchAll : Maybe String -> Cmd Msg
fetchAll token =
    Cmd.batch
        [ Api.fetchLibrary token GotLibrary
        , Api.fetchContinueWatching token GotContinueWatching
        , Api.fetchPopular token GotPopular
        , Api.fetchNew token GotNew
        , fetchRequestsData token
        ]


{-| Re-fetch the requests queue, the tracker list and the TMDB popular
sections (m20a/m20b/m26). -}
fetchRequestsData : Maybe String -> Cmd Msg
fetchRequestsData token =
    Cmd.batch
        [ Api.fetchRequests token GotRequests
        , Api.fetchTrackers token GotTrackers
        , Api.fetchDiscoverPopular token GotDiscoverPopular
        ]


{-| m26: the indexer search command - with no tracker selected (Nothing or
the ALL TRACKERS empty option) the server searches ALL enabled indexers. -}
searchIndexersCmd : Model -> Cmd Msg
searchIndexersCmd model =
    let
        tid =
            case model.activeTracker of
                Just "" ->
                    Nothing

                other ->
                    other
    in
    Api.searchIndexers model.token tid model.reqQuery GotReleases


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
