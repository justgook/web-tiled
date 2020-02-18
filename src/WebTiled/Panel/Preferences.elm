module WebTiled.Panel.Preferences exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import WebTiled.Message exposing (Message(..), PreferencesTab(..))
import WebTiled.Panel.Preferences.Categories as Categories


view : PreferencesTab -> List (Html Message) -> Html Message
view category content =
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
                , div [ class "pane" ] content
                ]
            ]
        , footer [ class "toolbar toolbar-footer" ]
            [ div [ class "toolbar-actions" ]
                [ button [ onClick CloseModal, class "btn btn-default pull-right" ] [ text "Close" ] ]
            ]
        ]
