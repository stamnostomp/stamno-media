module Types exposing
    ( AuthUser
    , Model
    , Msg(..)
    , NowPlaying(..)
    , Screen(..)
    , Tab(..)
    , VideoState
    , findLibraryItem
    , introRange
    , nextEpisodeItem
    , init
    , initialVideoState
    )

import Api
import Browser.Dom as Dom
import Catalog exposing (Genre)
import Http
import Task


type Screen
    = Home
    | Movies
    | Shows
    | Library
    | Detail
    | RealDetail
    | Player
    | Requests
    | SeriesView String


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
'Ports.videoStatus' port.
-}
type alias VideoState =
    { currentTime : Float
    , duration : Float
    , paused : Bool
    , muted : Bool
    , volume : Float
    }


{-| Assume paused until a real status event says otherwise: most browsers
block unmuted `autoplay`, so starting from `paused = False` here would
desync the play/pause button and icon from the actual (likely still
paused) video until the first status event arrives.
-}
initialVideoState : VideoState
initialVideoState =
    { currentTime = 0, duration = 0, paused = True, muted = False, volume = 1 }


{-| The logged-in user, from the `User` object of the auth response. -}
type alias AuthUser =
    { name : String
    , isAdmin : Bool
    }


type alias Model =
    { screen : Screen
    , activeId : Int
    , filter : Maybe Genre
    , search : String
    , requestsTab : Tab
    , myRequests : List Api.RequestEntry
    , trackers : List Api.Tracker
    , activeTracker : Maybe String
    , reqQuery : String
    , reqTmdbId : Maybe Int
    , reqYear : Maybe Int
    , releases : List Api.Release
    , reqNotice : Maybe String
    , reqLoading : Bool
    , reqPollAcc : Float
    , popularMovies : List Api.DiscoverItem
    , popularShows : List Api.DiscoverItem
    , searchMovies : List Api.DiscoverItem
    , searchShows : List Api.DiscoverItem
    , time : Float
    , resolution : { width : Float, height : Float }
    , library : List Api.Item
    , libraryError : Maybe String
    , continueWatching : List Api.ContinueItem
    , popular : List Api.Item
    , newItems : List Api.Item
    , nowPlaying : NowPlaying
    , playSrc : Maybe String
    , subtitles : List Api.SubtitleEntry
    , chapters : List Api.Chapter
    , ccOn : Bool
    , videoState : VideoState
    , controlsVisible : Bool
    , idleTime : Float
    , resumeProgress : Maybe Int
    , previousScreen : Screen
    , activeRealItem : Maybe Api.Item
    , itemMeta : Maybe Api.ItemMetadata
    , movieGenreFilter : Maybe String
    , movieSort : String
    , seriesHits : List Api.SeriesHit
    , seriesDetail : Maybe Api.SeriesDetail
    , seriesSeason : Maybe Int
    , discoverShow : Maybe Api.DiscoverItem
    , userMenuOpen : Bool
    , token : Maybe String
    , authUser : Maybe AuthUser
    , needsAuth : Bool
    , loginUser : String
    , loginPass : String
    , loginError : Maybe String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { screen = Home
      , activeId = 1
      , filter = Nothing
      , search = ""
      , requestsTab = Mine
      , myRequests = []
      , trackers = []
      , activeTracker = Nothing
      , reqQuery = ""
      , reqTmdbId = Nothing
      , reqYear = Nothing
      , releases = []
      , reqNotice = Nothing
      , reqLoading = False
      , reqPollAcc = 0
      , popularMovies = []
      , popularShows = []
      , searchMovies = []
      , searchShows = []
      , time = 0
      , resolution = { width = 1920, height = 1080 }
      , library = []
      , libraryError = Nothing
      , continueWatching = []
      , popular = []
      , newItems = []
      , nowPlaying = DemoPlaying
      , playSrc = Nothing
      , subtitles = []
      , chapters = []
      , ccOn = False
      , videoState = initialVideoState
      , controlsVisible = True
      , idleTime = 0
      , resumeProgress = Nothing
      , previousScreen = Home
      , activeRealItem = Nothing
      , itemMeta = Nothing
      , movieGenreFilter = Nothing
      , movieSort = "az"
      , seriesHits = []
      , seriesDetail = Nothing
      , seriesSeason = Nothing
      , discoverShow = Nothing
      , userMenuOpen = False
      , token = Nothing
      , authUser = Nothing
      , needsAuth = False
      , loginUser = ""
      , loginPass = ""
      , loginError = Nothing
      }
    , Cmd.batch
        [ Task.perform GotViewport Dom.getViewport
        , Api.fetchLibrary Nothing GotLibrary
        , Api.fetchContinueWatching Nothing GotContinueWatching
        , Api.fetchPopular Nothing GotPopular
        , Api.fetchNew Nothing GotNew
        , Api.fetchRequests Nothing GotRequests
        , Api.fetchTrackers Nothing GotTrackers
        ]
    )


type Msg
    = NavigateTo Screen
    | OpenDetail Int
    | OpenPlayer
    | OpenRealDetail Api.Item
    | GotItemMetadata (Result Http.Error Api.ItemMetadata)
    | OpenSeries String
    | GotSeriesSearch (Result Http.Error (List Api.SeriesHit))
    | GotSeriesEpisodes (Result Http.Error Api.SeriesDetail)
    | SetSeriesSeason Int
    | OpenRealPlayer Api.Item
    | GotPlaybackInfo (Result Http.Error Api.PlaybackInfo)
    | GotSubtitles (Result Http.Error (List Api.SubtitleEntry))
    | GotChapters (Result Http.Error (List Api.Chapter))
    | SkipIntro
    | PlayNextEpisode
    | ToggleCC
    | BackFromPlayer
    | SetFilter (Maybe Genre)
    | SetMovieGenreFilter (Maybe String)
    | CycleMovieSort
    | SetSearch String
    | SetTab Tab
    | GotRequests (Result Http.Error (List Api.RequestEntry))
    | GotTrackers (Result Http.Error (List Api.Tracker))
    | SetReqQuery String
    | SearchReleases
    | GotReleases (Result Http.Error (List Api.Release))
    | CreateRelease Api.Release
    | GotCreateRequest Int (Result Http.Error Api.RequestEntry)
    | ClearReqNotice
    | GotDiscoverPopular (Result Http.Error ( List Api.DiscoverItem, List Api.DiscoverItem ))
    | GotDiscoverSearch (Result Http.Error ( List Api.DiscoverItem, List Api.DiscoverItem ))
    | PickPopular Api.DiscoverItem
    | PickShow Api.DiscoverItem
    | BackToDiscoverCards
    | BackToDiscoverEpisodes
    | DownloadEpisode String Int Int
    | DownloadSeason String Int
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
    | ToggleUserMenu
    | CloseUserMenu
    | GotStoredToken String
    | SetLoginUser String
    | SetLoginPass String
    | SubmitLogin
    | GotAuth (Result Http.Error Api.AuthResult)
    | SignOut
    | GotUnauthorized
    | NoOp


{-| The library Episode item matching series (case-insensitive) + S/E --
the same matching the server uses for the Downloaded flag. Shared by the
series page and the player's next-episode (m74). -}
findLibraryItem : List Api.Item -> String -> Int -> Int -> Maybe Api.Item
findLibraryItem library seriesName season ep =
    List.head
        (List.filter
            (\i ->
                String.toLower i.itemType == "episode"
                    && String.toLower (Maybe.withDefault "" i.seriesName) == String.toLower seriesName
                    && i.itemSeason == Just season
                    && i.itemEpisode == Just ep
            )
            library
        )


{-| m74: the intro range of an item as (startSeconds, endSeconds), if any.
A chapter NAMED "intro"/"opening" wins (real chapter markers); otherwise,
for episodes longer than 10 minutes whose first chapter starts at 0 (the
server falls back to evenly spaced unnamed chapters), the first chapter is
treated as the intro. -}
introRange : Bool -> List Api.Chapter -> Float -> Maybe (Float, Float)
introRange isEpisode chapters duration =
    let
        secs c =
            toFloat c.startPositionTicks / 1e7

        isIntroName n =
            let
                l =
                    String.toLower n
            in
            String.contains "intro" l || String.contains "opening" l

        chapterEnd c =
            case List.head (List.filter (\n -> secs n > secs c) chapters) of
                Just n ->
                    secs n

                Nothing ->
                    duration

        named =
            List.filter (\c -> Maybe.withDefault False (Maybe.map isIntroName c.name)) chapters
    in
    case named of
        (c :: _) ->
            Just (secs c, chapterEnd c)

        [] ->
            case chapters of
                (c :: _) ->
                    if isEpisode && duration > 600 && secs c <= 0.001 then
                        Just (0, chapterEnd c)

                    else
                        Nothing

                _ ->
                    Nothing


{-| m74: the next DOWNLOADED episode of the same series after the given
item, resolved to a library item (so it can be played), or Nothing. -}
nextEpisodeItem : List Api.Item -> Maybe Api.SeriesDetail -> Api.Item -> Maybe Api.Item
nextEpisodeItem library mDetail item =
    case ( item.seriesName, item.itemSeason, item.itemEpisode ) of
        ( Just sn, Just se, Just ep ) ->
            case mDetail of
                Just detail ->
                    let
                        downloaded =
                            detail.seasons
                                |> List.filter (\s -> s.seasonNumber > 0)
                                |> List.concatMap
                                    (\s ->
                                        s.episodes
                                            |> List.filter .downloaded
                                            |> List.map (\e -> ( s.seasonNumber, e.episodeNumber ))
                                    )
                                |> List.sort

                        after =
                            List.filter (\( s2, e2 ) -> ( s2, e2 ) > ( se, ep )) downloaded
                    in
                    after
                        |> List.head
                        |> Maybe.andThen (\( s2, e2 ) -> findLibraryItem library sn s2 e2)

                Nothing ->
                    Nothing

        _ ->
            Nothing
