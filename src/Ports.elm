port module Ports exposing (scrollRow, videoCommand, videoStatus)


port scrollRow : { rowId : String, delta : Float } -> Cmd msg


{-| Commands sent down to the JS side to drive the real `<video>` element
(Elm has no built-in media-element bindings). `action` is one of "play",
"pause", "seek", "skip", "mute", "volume", "fullscreen"; `value` carries
the seek target / skip delta / volume level and is ignored otherwise.
-}
port videoCommand : { action : String, value : Float } -> Cmd msg


{-| Status pushed up from the JS side on every timeupdate/play/pause/
volumechange event of the real `<video>` element.
-}
port videoStatus : ({ currentTime : Float, duration : Float, paused : Bool, muted : Bool, volume : Float } -> msg) -> Sub msg
