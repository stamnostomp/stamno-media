module Pages.Movies exposing (view)

import Api
import Components exposing (libraryCard)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Types exposing (Model, Msg)


{-| m41/m43: real server library items of type "movie" (fallback: video
mime without a series), searched by the global search box. -}
view : Model -> Html Msg
view model =
    let
        movies =
            model.library
                |> List.filter isMovie
                |> search model.search
    in
    div [ class "pa4" ]
        [ div [ class "text-xl ttu tracked-wide mb-5 fg-primary" ] [ text "MOVIES" ]
        , if List.isEmpty movies then
            div [ class "f6 fg-muted" ] [ text "No movies found." ]

          else
            div [ class "grid-fill" ] (List.map (libraryCard model.token) movies)
        ]


isMovie : Api.Item -> Bool
isMovie i =
    i.itemType == "movie"
        || (i.itemType == "" && String.startsWith "video/" i.mime && i.seriesName == Nothing)


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
