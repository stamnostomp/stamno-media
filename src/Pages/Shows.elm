module Pages.Shows exposing (view)

import Api
import Components exposing (libraryCard)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Types exposing (Model, Msg)


{-| m41: real server items that are not video (audio/other), searched by
the global search box. When the library has none, a muted note shows. -}
view : Model -> Html Msg
view model =
    let
        shows =
            model.library
                |> List.filter (\i -> not (String.startsWith "video/" i.mime))
                |> search model.search
    in
    div [ class "pa4" ]
        [ div [ class "text-xl ttu tracked-wide mb-5 fg-primary" ] [ text "SHOWS" ]
        , if List.isEmpty shows then
            div [ class "f6 fg-muted" ] [ text "NO SHOWS YET" ]

          else
            div [ class "grid-fill" ] (List.map (libraryCard model.token) shows)
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
