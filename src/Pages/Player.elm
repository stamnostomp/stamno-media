module Pages.Player exposing (view)

import Api
import Catalog exposing (Title)
import Html exposing (Html, div, input, span, text)
import Html.Attributes as Attr exposing (autoplay, class, id, src, style)
import Html.Events as Events exposing (onClick)
import Json.Decode as Decode
import Types exposing (Model, Msg(..), NowPlaying(..), VideoState, nextEpisodeItem)


view : Model -> Html Msg
view model =
    case model.nowPlaying of
        RealPlaying item ->
            -- m32: pass the token so the <video src> can carry api_key=
            -- m74: skip-intro range + next downloaded episode for episodes
            viewReal item model.token model.playSrc model.videoState model.controlsVisible model.resumeProgress model.subtitles model.ccOn model.intro (nextEpisodeItem model.library model.seriesDetail item) model.playDuration

        DemoPlaying ->
            viewDemo model.activeId


{-| Actual playback of a file streamed from the media server, via a
`<video>` element hitting `GET /stream/:id` (byte-range requests and all).
The video fills the whole viewport; the back link and control bar are
absolutely-positioned overlays on top of it, each with a gradient behind
them for legibility. Native controls are disabled; play/pause/seek/mute/
fullscreen are driven through the `videoCommand` port. The play/pause
toggle lives on the `<video>` element itself (not an ancestor), so clicks
on the header/control overlays don't also toggle playback.

The whole stage (not just the `<video>`) is what goes fullscreen — see the
'fullscreen' case in index.html's port glue — since the Fullscreen API only
renders the fullscreened element's own descendants; fullscreening the video
alone would drop these overlays entirely. Because show/hide of the overlays
is driven by this same `controlsVisible` state either way, behaviour is
identical windowed or fullscreen.

If `maybeResumeProgress` is set (the item appears in continue-watching),
the video carries a `data-resume-percent` attribute that index.html's JS
reads once, on `loadedmetadata`, to seek to that position before playback
is visible to the user.
-}
viewReal : Api.Item -> Maybe String -> Maybe String -> VideoState -> Bool -> Maybe Int -> List Api.SubtitleEntry -> Bool -> Maybe ( Float, Float ) -> Maybe Api.Item -> Maybe Float -> Html Msg
viewReal item token playSrc videoState controlsVisible maybeResumeProgress subtitles ccOn intro nextEp realDur =
    div
        [ id "player-stage"
        , class "fixed top-0 left-0 w-100 vh-100 overflow-hidden"
        , style "background" "black"
        , Events.on "mousemove" (Decode.succeed PlayerActivity)
        ]
        [ Html.node "video"
            [ id "media-player"
            , Attr.attribute "data-item-id" item.id
            , Attr.attribute "data-resume-percent"
                (maybeResumeProgress |> Maybe.map String.fromInt |> Maybe.withDefault "")
            , Attr.attribute "data-real-duration"
                (realDur |> Maybe.map String.fromFloat |> Maybe.withDefault "")
            , autoplay True
            , src (Maybe.withDefault (Api.streamUrl token item.id) playSrc)
            , onClick VideoTogglePlay
            , style "position" "absolute"
            , style "top" "0"
            , style "left" "0"
            , style "width" "100%"
            , style "height" "100%"
            , style "object-fit" "contain"
            ]
            (trackNodes token item.id subtitles)
        , viewPauseOverlay videoState.paused
        , viewSkipIntro intro videoState
        , viewNextEpisodeOverlay nextEp videoState
        , div
            [ class "absolute top-0 left-0 w-100 flex items-center justify-between pv3 ph4"
            , style "background" "linear-gradient(180deg, rgba(0,0,0,0.75), transparent)"
            , style "opacity" (opacityFor controlsVisible)
            , style "pointer-events" (pointerEventsFor controlsVisible)
            , style "transition" "opacity 0.3s"
            ]
            [ div [ class "f6 tracked-wide ttu" ] [ text item.displayTitle ]
            , div [ class "flex items-center gap-3" ]
                [ -- m33: TRANSCODING badge when the server told us to transcode
                  case playSrc of
                    Just _ ->
                        div [ class "ba b--default f7 tracked-wide ttu fg-muted", style "padding" "2px 8px" ] [ text "TRANSCODING" ]

                    Nothing ->
                        text ""
                , div [ class "pointer f6 fg-secondary", onClick BackFromPlayer ] [ text "← BACK" ]
                ]
            ]
        , div
            [ class "absolute bottom-0 left-0 w-100 ph-7 pv4 flex flex-column gap-3 fade-up-strong"
            , style "opacity" (opacityFor controlsVisible)
            , style "pointer-events" (pointerEventsFor controlsVisible)
            , style "transition" "opacity 0.3s"
            ]
            [ videoSeekBar videoState
            , div [ class "flex items-center justify-between" ]
                [ div [ class "flex items-center gap-5 text-md" ]
                    [ span [ class "pointer", onClick (VideoSkip -10) ] [ text "◄◄" ]
                    , viewPlayPauseButton videoState.paused
                    , span [ class "pointer", onClick (VideoSkip 10) ] [ text "►►" ]
                    , span [ class "f7 fg-muted tracked-wide" ]
                        [ text (formatTime videoState.currentTime ++ " / " ++ formatTime videoState.duration) ]
                    ]
                , div [ class "flex items-center gap-5 f6 fg-secondary" ]
                    [ volControl videoState
                    , span
                        [ class "pointer"
                        , style "opacity" (if ccOn then "1" else "0.4")
                        , onClick ToggleCC
                        ]
                        [ text "CC" ]
                    , case nextEp of
                        Just n ->
                            span
                                [ class "pointer fg-primary"
                                , onClick PlayNextEpisode
                                , Attr.title (nextEpisodeLabel n)
                                ]
                                [ text ("NEXT ▸ " ++ nextEpisodeLabel n) ]

                        Nothing ->
                            text ""
                    , span [ class "pointer", onClick VideoFullscreen ] [ text "⛶" ]
                    ]
                ]
            ]
        ]


{-| m70: one <track> element per subtitle source of the item. English is
the default track when present, else the first source. -}
trackNodes : Maybe String -> String -> List Api.SubtitleEntry -> List (Html Msg)
trackNodes token itemId subs =
    let
        defaultIndex =
            case List.head (List.filter (\s -> s.language == Just "eng") subs) of
                Just s ->
                    s.index

                Nothing ->
                    case List.head subs of
                        Just s ->
                            s.index

                        Nothing ->
                            -1
    in
    List.map
        (\s ->
            Html.node "track"
                ([ Attr.attribute "kind" "subtitles"
                 , Attr.attribute "src" (Api.subtitleUrl token itemId s.index)
                 , Attr.attribute "srclang" (Maybe.withDefault "und" s.language)
                 , Attr.attribute "label"
                    (case s.language of
                        Just l ->
                            languageName l  -- m89: "eng" -> "English", never the file path

                        Nothing ->
                            "Sub " ++ String.fromInt s.index
                    )
                 ]
                    ++ (if s.index == defaultIndex then
                            [ Attr.attribute "default" "" ]

                        else
                            []
                       )
                )
                []
        )
        subs



viewPauseOverlay : Bool -> Html Msg
viewPauseOverlay paused =
    if not paused then
        text ""

    else
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


{-| A hidden bar must not intercept clicks meant for the video underneath,
hence tying pointer-events to the same visibility flag as opacity. -}
opacityFor : Bool -> String
opacityFor visible =
    if visible then
        "1"

    else
        "0"


pointerEventsFor : Bool -> String
pointerEventsFor visible =
    if visible then
        "auto"

    else
        "none"


viewPlayPauseButton : Bool -> Html Msg
viewPlayPauseButton paused =
    span
        [ class "pointer tc"
        , style "display" "inline-block"
        , style "width" "1.25em"
        , style "font-size" "1.75rem"
        , onClick VideoTogglePlay
        ]
        [ text
            (if paused then
                "▶"

             else
                "❚❚"
            )
        ]


{-| Clickable/scrubbable progress bar: click position (as a fraction of the
bar's width) maps directly to a seek target.
-}
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
popup open first.
-}
volControl : VideoState -> Html Msg
volControl videoState =
    div
        [ class "vol-control relative pointer"
        , Events.preventDefaultOn "wheel" wheelVolumeDecoder
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
                [ Attr.type_ "range"
                , Attr.min "0"
                , Attr.max "1"
                , Attr.step "0.01"
                , Attr.value
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
    Events.on "input"
        (Events.targetValue
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
`event.target` here is always the bar itself, never the overlay —
otherwise a click landing on the (already-played) filled portion would
compute its offset and width against the overlay's own box instead of
the full bar.
-}
onClickFraction : (Float -> Msg) -> Html.Attribute Msg
onClickFraction toMsg =
    Events.on "click"
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


{-| Styled mockup player: no real file behind it, all controls decorative. -}
viewDemo : Int -> Html Msg
viewDemo activeId =
    let
        active =
            Catalog.find activeId
    in
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



{-| m89: ISO 639 code -> readable language name for the CC menu. -}
languageName : String -> String
languageName code =
    case String.toLower code of
        "eng" -> "English"
        "spa" -> "Spanish"
        "por" -> "Portuguese"
        "fra" -> "French"
        "deu" -> "German"
        "ita" -> "Italian"
        "jpn" -> "Japanese"
        "kor" -> "Korean"
        "zho" -> "Chinese"
        "rus" -> "Russian"
        "nld" -> "Dutch"
        "swe" -> "Swedish"
        "und" -> "Unknown"
        other -> String.toUpper other


{-| m75: the "SKIP INTRO" button while the playhead is inside the intro
range reported by the server (GET /Items/:id/Intro). Plain label only. -}
viewSkipIntro : Maybe ( Float, Float ) -> VideoState -> Html Msg
viewSkipIntro intro videoState =
    case intro of
        Just ( start, end ) ->
            if videoState.duration > 0 && videoState.currentTime >= start && videoState.currentTime < end - 1.5 then
                div
                    [ class "absolute"
                    , style "right" "28px"
                    , style "bottom" "110px"
                    , style "z-index" "5"
                    ]
                    [ Html.button
                        [ class "btn btn-primary f7 ttu tracked-wide"
                        , style "padding" "8px 16px"
                        , onClick SkipIntro
                        ]
                        [ text "SKIP INTRO" ]
                    ]

            else
                text ""

        Nothing ->
            text ""


{-| m74: "up next" panel shown in the last 30s (or when playback ended) when
a next downloaded episode exists. -}
viewNextEpisodeOverlay : Maybe Api.Item -> VideoState -> Html Msg
viewNextEpisodeOverlay nextEp videoState =
    let
        nearEnd =
            videoState.duration > 0 && videoState.currentTime >= videoState.duration - 30

        ended =
            videoState.duration > 0 && videoState.paused && videoState.currentTime >= videoState.duration - 0.75
    in
    case nextEp of
        Just n ->
            if nearEnd || ended then
                div
                    [ class "absolute flex items-center gap-4 pa3"
                    , style "right" "28px"
                    , style "bottom" "110px"
                    , style "z-index" "5"
                    , style "background" "rgba(0,0,0,0.75)"
                    , style "border" "1px solid var(--default)"
                    ]
                    [ div [ class "flex flex-column" ]
                        [ span [ class "f7 fg-muted ttu tracked-wide" ] [ text "UP NEXT" ]
                        , span [ class "f6 fg-primary" ] [ text (nextEpisodeLabel n) ]
                        ]
                    , Html.button
                        [ class "btn btn-primary f7 ttu tracked-wide"
                        , style "padding" "8px 16px"
                        , onClick PlayNextEpisode
                        ]
                        [ text "▶ PLAY NEXT" ]
                    ]

            else
                text ""

        Nothing ->
            text ""


{-| m74: "S25E01" style label for the next episode. -}
nextEpisodeLabel : Api.Item -> String
nextEpisodeLabel n =
    "S" ++ String.fromInt (Maybe.withDefault 0 n.itemSeason) ++ "E" ++ String.fromInt (Maybe.withDefault 0 n.itemEpisode)
