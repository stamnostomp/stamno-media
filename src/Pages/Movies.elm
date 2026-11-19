module Pages.Movies exposing (view)

import Api
import Components exposing (libraryCard)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Types exposing (Model, Msg)


{-| m41: real server library items of movie-ish (video/*) mime, searched
by the global search box; rendered with the standard poster cards. -}
view : Model -> Html Msg
view model =
    let
        movies =
            model.library
                |> List.filter (\i -> String.startsWith "video/" i.mime)
                |> search model.search
    in
    div [ class "pa4" ]
        [ div [ class "text-xl ttu tracked-wide mb-5 fg-primary" ] [ text "MOVIES" ]
        , if List.isEmpty movies then
            div [ class "f6 fg-muted" ] [ text "No movies found." ]

          else
            div [ class "grid-fill" ] (List.map (libraryCard model.token) movies)
        ]


search : String -> List Api.Item -> List Api.Item
search query items =
    if String.isEmpty query then
        items

    else
        let
            lower =
                String.toLower query
        in
        List.filter (\item -> String.contains lower (String.toLower item.displayTitle) || String.contains lower (String.toLower item.title)) items
