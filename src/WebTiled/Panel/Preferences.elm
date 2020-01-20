module WebTiled.Panel.Preferences exposing (Model, init, view)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import WebTiled.Message exposing (PreferencesTab)
import WebTiled.Panel.Preferences.Categories as Categories
import WebTiled.Panel.Preferences.Keyboard as Keyboard
import WebTiled.RenameMe


type alias Model =
    { category : PreferencesTab }


init =
    { category = Categories.init }


view { category } =
    let
        keyboardModel =
            WebTiled.RenameMe.shortcuts
                |> Dict.foldl (\k v acc -> { name = k, context = "Global", shortcut = v.shortcut } :: acc) []
    in
    div [ class "window" ]
        [ header [ class "toolbar toolbar-header" ]
            [ div [ class "toolbar-actions" ]
                [ div [ class "btn-group pull-right" ]
                    [ label [ class "toolbar-input" ]
                        [ span [ class "icon icon-search" ]
                            []
                        , input [ class "form-control", placeholder "Search...", type_ "search" ]
                            []
                        ]
                    ]
                ]
            ]
        , div [ class "window-content" ]
            [ div [ class "pane-group" ]
                [ div [ class "pane pane-sm sidebar" ] [ Categories.view category ]
                , div [ class "pane" ] [ keyboardPresets, Keyboard.view keyboardModel ]
                ]
            ]
        , footer [ class "toolbar toolbar-footer" ]
            [ div [ class "toolbar-actions" ]
                [ button [ class "btn btn-default pull-right" ]
                    [ text "Close" ]
                ]
            ]
        ]


keyboardPresets =
    div [ class " padded-less" ]
        [ select [ class "form-control" ]
            [ option [] [ text "Custom..." ]
            , option [] [ text "MacOS" ]
            , option [] [ text "Windows" ]
            ]
        ]
