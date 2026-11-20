module Types exposing
    ( AuthUser
    , Model
    , Msg(..)
    , NowPlaying(..)
    , Screen(..)
    , Tab(..)
    , VideoState
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
