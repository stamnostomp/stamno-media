module Main exposing (main)

import Browser
import Browser.Events
import Ports
import Types exposing (Msg(..))
import Update
import View


main : Program () Types.Model Msg
main =
    Browser.document
        { init = Types.init
        , update = Update.update
        , view = View.view
        , subscriptions =
            \_ ->
                Sub.batch
                    [ Browser.Events.onAnimationFrameDelta Tick
                    , Browser.Events.onResize WindowResized
                    , Ports.videoStatus GotVideoStatus
                    , Ports.tokenStored GotStoredToken
                    ]
        }
