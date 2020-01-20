module WebTiled.Panel.Preferences.Keyboard exposing (Model, view)

import Html exposing (..)
import Html.Attributes exposing (..)


type alias Model =
    List { name : String, context : String, shortcut : String }


init : Model
init =
    []


view : Model -> Html msg
view model =
    table [ class "table-striped" ]
        [ thead []
            [ tr []
                [ th [] [ text "Action" ]
                , th [ class "text-center" ] [ text "Context" ]
                , th [ class "text-center" ] [ text "Shortcut" ]
                ]
            ]
        , model
            |> List.map
                (\{ name, context, shortcut } ->
                    tr []
                        [ td [] [ text name ]
                        , td [ class "text-center" ] [ text context ]
                        , td [ class "text-center" ] [ text shortcut ]
                        ]
                )
            |> tbody []
        ]
