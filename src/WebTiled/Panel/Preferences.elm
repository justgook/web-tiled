module WebTiled.Panel.Preferences exposing (view)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import WebTiled.Message exposing (Message(..), PreferencesTab(..))
import WebTiled.Panel.Preferences.Account
import WebTiled.Panel.Preferences.Categories as Categories
import WebTiled.Panel.Preferences.Keyboard as Keyboard
import WebTiled.RenameMe


view : PreferencesTab -> Html Message
view category =
    div [ class "window" ]
        [ header [ class "toolbar toolbar-header" ]
            [ div [ class "toolbar-actions" ]
                [ div [ class "btn-group pull-right" ]
                    [ label [ class "toolbar-input" ]
                        [ span [ class "icon icon-search" ] []
                        , input [ class "form-control", placeholder "Search...", type_ "search" ] []
                        ]
                    ]
                ]
            ]
        , div [ class "window-content" ]
            [ div [ class "pane-group" ]
                [ div [ class "pane pane-sm sidebar" ] [ Categories.view category ]
                , div [ class "pane" ] (content category)
                ]
            ]
        , footer [ class "toolbar toolbar-footer" ]
            [ div [ class "toolbar-actions" ]
                [ button [ onClick CloseModal, class "btn btn-default pull-right" ] [ text "Close" ] ]
            ]
        ]


content category =
    let
        keyboardModel =
            WebTiled.RenameMe.shortcuts
                |> Dict.foldl (\k v acc -> { name = k, context = "Global", shortcut = v.shortcut } :: acc) []
    in
    case category of
        Keyboard ->
            [ div [ class " padded-less" ]
                [ select [ class "form-control" ]
                    [ option [] [ text "Custom..." ]
                    , option [] [ text "MacOS" ]
                    , option [] [ text "Windows" ]
                    ]
                ]
            , Keyboard.view keyboardModel
            ]

        Account ->
            [ WebTiled.Panel.Preferences.Account.view ]

        _ ->
            [ div [] [ text "Not Implemented" ] ]
