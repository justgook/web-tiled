module WebTiled.Panel.Preferences.Account exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)


view : Html msg
view =
    [ div [ class "form-group" ]
        [ label [] [ text "Connect your storage" ]
        , input [ class "form-control", placeholder "user@provider.com", type_ "email" ] []
        ]
    , button [ class "btn btn-large btn-positive" ] [ text "Connect" ]
    ]
        |> div [ class "pane padded" ]
