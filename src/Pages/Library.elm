module Pages.Library exposing (view)

import Api
import Components exposing (libraryCard)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Types exposing (Model, Msg)


view : String -> Model -> Html Msg
view pageTitle model =
    let
        results =
            search model.search model.library
    in
    div [ class "pa4" ]
        [ div [ class "text-xl ttu tracked-wide mb-5 fg-primary" ] [ text pageTitle ]
        , case model.libraryError of
            Just err ->
                div [ class "f6 fg-muted mb4" ] [ text ("Could not reach media server: " ++ err) ]

            Nothing ->
                text ""
        , if List.isEmpty results then
            div [ class "f6 fg-muted" ]
                [ text
                    (if model.libraryError /= Nothing then
                        ""

                     else
                        "No media found."
                    )
                ]

          else
            div [ class "grid-fill" ] (List.map (libraryCard model.token) results)
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
        List.filter (\item -> String.contains lower (String.toLower item.title)) items
