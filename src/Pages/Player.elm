module Pages.Player exposing (view)

import Api
import Catalog exposing (Title)
import Html exposing (Html, div, input, span, text)
import Html.Attributes as Attr exposing (autoplay, class, id, src, style)
import Html.Events as Events exposing (onClick)
import Json.Decode as Decode
import Types exposing (Model, Msg(..), NowPlaying(..), VideoState)


view : Model -> Html Msg
view model =
    case model.nowPlaying of
        RealPlaying item ->
            viewReal item model.videoState model.controlsVisible model.resumeProgress

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
viewReal : Api.Item -> VideoState -> Bool -> Maybe Int -> Html Msg
viewReal item videoState controlsVisible maybeResumeProgress =
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
        , viewPauseOverlay videoState.paused
        , div
            [ class "absolute top-0 left-0 w-100 flex items-center justify-between pv3 ph4"
            , style "background" "linear-gradient(180deg, rgba(0,0,0,0.75), transparent)"
            , style "opacity" (opacityFor controlsVisible)
            , style "pointer-events" (pointerEventsFor controlsVisible)
            , style "transition" "opacity 0.3s"
            ]
            [ div [ class "f6 tracked-wide ttu" ] [ text item.title ]
            , div [ class "pointer f6 fg-secondary", onClick BackFromPlayer ] [ text "← BACK" ]
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
                    , span [ class "pointer" ] [ text "CC" ]
                    , span [ class "pointer", onClick VideoFullscreen ] [ text "⛶" ]
                    ]
                ]
            ]
        ]


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
