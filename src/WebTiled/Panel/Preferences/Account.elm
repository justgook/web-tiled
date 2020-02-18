module WebTiled.Panel.Preferences.Account exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import WebTiled.Message exposing (Message(..))
import WebTiled.Model exposing (RemoteStorageStatus(..))


view : RemoteStorageStatus -> String -> Html Message
view status userName =
    [ div [ class "form-group" ]
        [ label [] [ text "Connect your storage" ]
        , input
            [ class "form-control"
            , value userName
            , placeholder "user@provider.com"
            , type_ "email"
            , onInput RemoteStorageUserNameChange
            , disabled (status == Online || status == Connecting)
            ]
            []
        ]
    , case status of
        Online ->
            button
                [ class "btn btn-negative"
                , onClick RemoteStorageDisconnect
                ]
                [ text "Disconnect" ]

        Syncing ->
            button
                [ class "btn btn-warning"
                , disabled True
                ]
                [ text "Syncing" ]

        Offline ->
            button
                [ class "btn btn-large btn-positive"
                , onClick RemoteStorageConnect
                ]
                [ text "Connect" ]

        Connecting ->
            button
                [ class "btn btn-large btn-default"
                , onClick RemoteStorageConnect
                , disabled True
                ]
                [ text "Connecting..." ]
    ]
        |> div [ class "pane padded" ]
