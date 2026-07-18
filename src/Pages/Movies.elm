module Pages.Movies exposing (view)

import Html exposing (Html)
import Pages.Library
import Types exposing (Model, Msg)


{-| No real movie/show distinction exists in the API, so this is just the
real library under a different heading. -}
view : Model -> Html Msg
view =
    Pages.Library.view "MOVIES"
