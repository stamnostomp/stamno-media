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
            -- m55: episode cards (seriesName set) also resolve the series
            -- (search -> episodes) so the detail page can show the series view
            let
                seriesCmd =
                    case item.seriesName of
                        Just name ->
                            Api.fetchSeriesSearch model.token name GotSeriesSearch

                        Nothing ->
                            Cmd.none
            in
            ( { model
                | screen = RealDetail
                , activeRealItem = Just item
                , itemMeta = Nothing
                , seriesDetail = Nothing
                , seriesSeason = Nothing
                , previousScreen = model.screen
              }
              -- m34: fetch the stored TMDB metadata for the detail page
            , Cmd.batch
                [ Api.fetchItemMetadata model.token item.id GotItemMetadata
                , seriesCmd
                ]
            )

        GotItemMetadata result ->
            case result of
                Ok meta ->
                    ( { model | itemMeta = Just meta }, Cmd.none )

                Err err ->
                    if isUnauthorized err then
                        ( { model | needsAuth = True }, Cmd.none )

                    else
                        ( { model | itemMeta = Nothing }, Cmd.none )

        OpenSeries name ->
            -- m47: series view -- resolve the TMDB id, then fetch episodes
            ( { model | screen = SeriesView name, seriesHits = [], seriesDetail = Nothing, previousScreen = Shows }
            , Api.fetchSeriesSearch model.token name GotSeriesSearch
            )

        GotSeriesSearch result ->
            case result of
                Ok hits ->
                    let
                        cmd =
                            case List.head hits of
                                Just hit ->
                                    Api.fetchSeriesEpisodes model.token hit.id GotSeriesEpisodes

                                Nothing ->
                                    Cmd.none
                    in
                    ( { model | seriesHits = hits, needsAuth = False }, cmd )

                Err err ->
                    if isUnauthorized err then
                        ( { model | needsAuth = True }, Cmd.none )

                    else
                        ( { model | seriesHits = [] }, Cmd.none )

        SetSeriesSeason n ->
            ( { model | seriesSeason = Just n }, Cmd.none )

        GotSeriesEpisodes result ->
            case result of
                Ok detail ->
                    -- m54: default the season box to the season containing
                    -- the first downloaded episode, else the first season
                    ( { model
                        | seriesDetail = Just detail
                        , seriesSeason =
                            if model.seriesSeason == Nothing then
                                defaultSeriesSeason detail.seasons

                            else
                                model.seriesSeason
                        , reqLoading = False
                        , needsAuth = False
                      }
                    , Cmd.none
                    )

                Err err ->
                    if isUnauthorized err then
                        ( { model | needsAuth = True }, Cmd.none )

                    else
                        ( { model | seriesDetail = Nothing }, Cmd.none )

        OpenRealPlayer item ->
            ( { model
                | screen = Player
                , nowPlaying = RealPlaying item
                , playSrc = Nothing
                , subtitles = []
                , chapters = []
                , ccOn = False
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
              -- m33: ask PlaybackInfo whether to direct-play or transcode
            , Cmd.batch
                [ Api.fetchPlaybackInfo model.token item.id GotPlaybackInfo
                , Api.fetchSubtitles model.token item.id GotSubtitles
                , Api.fetchChapters model.token item.id GotChapters
                , -- m74: next-episode needs the series payload; fetch it when
                  -- the player was not opened from the series page
                  if isEpisodeItem item && not (seriesDetailMatches model item) then
                    Api.fetchSeriesSearch model.token (Maybe.withDefault "" item.seriesName) GotSeriesSearch

                  else
                    Cmd.none
                ]
            )

        GotSubtitles result ->
            case result of
                Ok subs ->
                    -- m70: default ON for English if present, else the first
                    -- track; the <track default> attribute shows it initially
                    ( { model | subtitles = subs, ccOn = True }, Cmd.none )

                Err err ->
                    if isUnauthorized err then
                        ( { model | needsAuth = True }, Cmd.none )

                    else
                        ( { model | subtitles = [] }, Cmd.none )

        GotChapters result ->
            case result of
                Ok chs ->
                    ( { model | chapters = chs }, Cmd.none )

                Err err ->
                    if isUnauthorized err then
                        ( { model | needsAuth = True }, Cmd.none )

                    else
                        ( { model | chapters = [] }, Cmd.none )

        SkipIntro ->
            -- m74: jump to the end of the intro range
            let
                isEp =
                    nowPlayingItem model |> Maybe.map isEpisodeItem |> Maybe.withDefault False
            in
            case Types.introRange isEp model.chapters model.videoState.duration of
                Just ( _, end ) ->
                    ( model, Ports.videoCommand { action = "seek", value = end } )

                Nothing ->
                    ( model, Cmd.none )

        PlayNextEpisode ->
            -- m74: play the next downloaded episode of the same series
            case nowPlayingItem model of
                Just item ->
                    case Types.nextEpisodeItem model.library model.seriesDetail item of
                        Just next ->
                            update (OpenRealPlayer next) model

                        Nothing ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ToggleCC ->
            -- m70: cycle off -> first track -> off (index = -1 means off)
            let
                n =
                    List.length model.subtitles

                next =
                    if not model.ccOn then
                        0

                    else
                        -1
            in
            ( { model | ccOn = not model.ccOn }
            , Ports.videoCommand { action = "subtitle", value = toFloat next }
            )

        GotPlaybackInfo result ->
            case result of
                Ok pi ->
                    -- m33: Transcode -> use the TranscodingUrl (with api_key);
                    -- DirectPlay -> playSrc stays Nothing (plain /stream/:id)
                    let
                        src =
                            if pi.playMethod == "Transcode" then
                                Maybe.map (Api.transcodingSrc model.token Api.baseUrl) pi.transcodingUrl

                            else
                                Nothing
                    in
                    ( { model | playSrc = src }, Cmd.none )

                Err err ->
                    if isUnauthorized err then
                        ( { model | needsAuth = True }, Cmd.none )

                    else
                        ( { model | playSrc = Nothing }, Cmd.none )

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

        SetMovieGenreFilter mg ->
            ( { model | movieGenreFilter = mg }, Cmd.none )

        CycleMovieSort ->
            -- az -> za -> new -> az
            let
                next =
                    case model.movieSort of
                        "az" ->
                            "za"

                        "za" ->
                            "new"

                        _ ->
                            "az"
            in
            ( { model | movieSort = next }, Cmd.none )

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
                    -- m59: no tracker picker anymore - searches always go
                    -- to ALL enabled indexers, so nothing is pre-selected
                    ( { model | trackers = ts, activeTracker = Nothing, needsAuth = False }, Cmd.none )

                Err err ->
                    if isUnauthorized err then
                        ( { model | needsAuth = True }, Cmd.none )

                    else
                        ( model, Cmd.none )

        SetReqQuery q ->
            ( { model | reqQuery = q }, Cmd.none )

        SearchReleases ->
            -- m59: Search shows TMDB result cards for the query (not the
            -- torrent list yet); clicking a card runs the indexer search
            ( { model | reqLoading = True, reqNotice = Nothing, releases = [], reqTmdbId = Nothing, reqYear = Nothing }
            , Api.fetchDiscoverSearch model.token model.reqQuery GotDiscoverSearch
            )

        PickPopular item ->
            -- m26/m59: picking a card (popular or search result) fills the
            -- query and searches ALL enabled indexers for it.
            -- m66: search with the display (English) name AND the TMDB
            -- original title, so original-language releases (e.g. Spanish
            -- "El laberinto del fauno" for Pan's Labyrinth) are found too
            let
                model2 =
                    { model
                        | reqQuery = item.name
                        , reqTmdbId = String.toInt item.id
                        , reqYear = item.year
                        , reqLoading = True
                        , reqNotice = Nothing
                        , searchMovies = []
                        , searchShows = []
                    }
            in
            ( model2, searchIndexersCmd model2 (searchQueries item.name item.originalName) )

        PickShow item ->
            -- m60: a SHOW card in Discover opens the series picker
            -- (season box + episode rows with DOWNLOAD buttons) instead of
            -- a flat list of every episode torrent
            ( { model
                | discoverShow = Just item
                , reqTmdbId = String.toInt item.id
                , reqYear = item.year
                , seriesDetail = Nothing
                , seriesSeason = Nothing
                , releases = []
                , searchMovies = []
                , searchShows = []
                , reqLoading = True
                , reqNotice = Nothing
              }
            , Api.fetchSeriesSearch model.token item.name GotSeriesSearch
            )

        BackToDiscoverCards ->
            ( { model
                | discoverShow = Nothing
                , reqTmdbId = Nothing
                , reqYear = Nothing
                , seriesDetail = Nothing
                , seriesSeason = Nothing
                , releases = []
                , searchMovies = []
                , searchShows = []
              }
            , Cmd.none
            )

        BackToDiscoverEpisodes ->
            ( { model | releases = [] }, Cmd.none )

        DownloadEpisode seriesName season ep ->
            -- m60: search all indexers for one episode (torznab season/ep).
            -- m66: also search with the show's original title (when the
            -- picked Discover card carried one) so original-language
            -- episode releases match too
            let
                q =
                    seriesName ++ " S" ++ pad2 season ++ "E" ++ pad2 ep

                qs =
                    List.map (\base -> base ++ " S" ++ pad2 season ++ "E" ++ pad2 ep)
                        (searchQueries seriesName (model.discoverShow |> Maybe.andThen .originalName))

                model2 =
                    { model
                        | reqQuery = q
                        , reqTmdbId = model.discoverShow |> Maybe.andThen (String.toInt << .id)
                        , reqYear = model.discoverShow |> Maybe.andThen .year
                        , releases = []
                        , reqLoading = True
                        , reqNotice = Nothing
                    }
            in
            ( model2, Api.searchIndexersTv model.token qs season (Just ep) GotReleases )

        DownloadSeason seriesName season ->
            -- m60: season-pack search (no ep param); m66: original title too
            let
                q =
                    seriesName ++ " S" ++ pad2 season

                qs =
                    List.map (\base -> base ++ " S" ++ pad2 season)
                        (searchQueries seriesName (model.discoverShow |> Maybe.andThen .originalName))

                model2 =
                    { model
                        | reqQuery = q
                        , reqTmdbId = model.discoverShow |> Maybe.andThen (String.toInt << .id)
                        , reqYear = model.discoverShow |> Maybe.andThen .year
                        , releases = []
                        , reqLoading = True
                        , reqNotice = Nothing
                    }
            in
            ( model2, Api.searchIndexersTv model.token qs season Nothing GotReleases )

        GotDiscoverPopular result ->
            case result of
                Ok ( movies, shows ) ->
                    ( { model | popularMovies = movies, popularShows = shows, needsAuth = False }, Cmd.none )

                Err err ->
                    if isUnauthorized err then
                        ( { model | needsAuth = True }, Cmd.none )

                    else
                        ( model, Cmd.none )

        -- m59: Search now fetches TMDB cards; clicking a card (PickPopular)
        -- runs the indexer search and shows the torrent list
        GotDiscoverSearch result ->
            case result of
                Ok ( movies, shows ) ->
                    ( { model | searchMovies = movies, searchShows = shows, reqLoading = False, needsAuth = False }, Cmd.none )

                Err err ->
                    if isUnauthorized err then
                        ( { model | needsAuth = True }, Cmd.none )

                    else
                        ( { model | reqLoading = False }, Cmd.none )

        GotReleases result ->
            case result of
                Ok rs ->
                    ( { model | releases = rs, reqLoading = False }, Cmd.none )

                Err err ->
                    if isUnauthorized err then
                        ( { model | needsAuth = True }, Cmd.none )

                    else
                        ( { model | releases = [], reqLoading = False, reqNotice = Just "SEARCH FAILED" }, Cmd.none )

        CreateRelease rel ->
            let
                payload =
                    { title =
                        if String.isEmpty model.reqQuery then
                            rel.title

                        else
                            model.reqQuery
                    , year = model.reqYear
                    , tmdbId = model.reqTmdbId
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

                Err err ->
                    if isUnauthorized err then
                        ( { model | needsAuth = True }, Cmd.none )

                    else
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

                -- m37: poll /Requests every ~5s while on the Requests tab so
                -- live Deluge progress bars update (skip while an action is
                -- in flight)
                onRequests =
                    model.screen == Requests

                acc0 =
                    if onRequests then
                        model.reqPollAcc + delta

                    else
                        0

                ( acc1, pollCmd ) =
                    if onRequests && acc0 >= 5000 && not model.reqLoading then
                        ( 0, Api.fetchRequests model.token GotRequests )

                    else
                        ( acc0, Cmd.none )
            in
            ( { model
                | time = model.time + delta * 0.001
                , idleTime = newIdleTime
                , controlsVisible = newControlsVisible
                , reqPollAcc = acc1
              }
            , pollCmd
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
                , searchMovies = []
                , searchShows = []
                , discoverShow = Nothing
                , seriesDetail = Nothing
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
            { id = item.id, title = item.title, size = item.size, mime = item.mime, progress = percent, displayTitle = item.displayTitle, itemType = item.itemType, seriesName = item.seriesName, itemSeason = item.itemSeason, itemEpisode = item.itemEpisode }
                :: withoutItem

        else
            withoutItem


{-| m54: the season to preselect on the series page: the first season that
has a downloaded episode, else the first season. -}
pad2 : Int -> String
pad2 n =
    if n < 10 then
        "0" ++ String.fromInt n

    else
        String.fromInt n


defaultSeriesSeason : List Api.SeriesSeason -> Maybe Int
defaultSeriesSeason seasons =
    case List.head (List.filter (\s -> List.any .downloaded s.episodes) seasons) of
        Just s ->
            Just s.seasonNumber

        Nothing ->
            List.head seasons |> Maybe.map .seasonNumber


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


{-| m59: the indexer search command - always searches ALL enabled
indexers server-side (the tracker dropdown was removed). m66: takes the
list of queries (English name + original title) sent as repeated q
params; the server merges the results. -}
searchIndexersCmd : Model -> List String -> Cmd Msg
searchIndexersCmd model queries =
    Api.searchIndexers model.token Nothing queries GotReleases


{-| m66: the indexer queries for a title: the display (usually English)
name plus the TMDB original title when it exists and differs -- so
torrent searches also cover the original language (Pan's Labyrinth ->
"El laberinto del fauno"). -}
searchQueries : String -> Maybe String -> List String
searchQueries name mOriginal =
    case mOriginal of
        Just orig ->
            if String.isEmpty orig || String.toLower orig == String.toLower name then
                [ name ]

            else
                [ name, orig ]

        Nothing ->
            [ name ]


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



{-| m74: the item currently in the real player, if any. -}
nowPlayingItem : Types.Model -> Maybe Api.Item
nowPlayingItem model =
    case model.nowPlaying of
        Types.RealPlaying item ->
            Just item

        _ ->
            Nothing


{-| m74: is this library item an episode? -}
isEpisodeItem : Api.Item -> Bool
isEpisodeItem item =
    String.toLower item.itemType == "episode"


{-| m74: does the loaded series detail belong to this episode's series? -}
seriesDetailMatches : Types.Model -> Api.Item -> Bool
seriesDetailMatches model item =
    case ( model.seriesDetail, item.seriesName ) of
        ( Just detail, Just sn ) ->
            String.toLower (Maybe.withDefault "" detail.name) == String.toLower sn

        _ ->
            False
