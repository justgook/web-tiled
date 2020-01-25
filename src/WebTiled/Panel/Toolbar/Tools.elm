module WebTiled.Panel.Toolbar.Tools exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)


view =
    header [ class "toolbar toolbar-header" ]
        [ div [ class "toolbar-actions" ]
            [ button [ class "btn btn-positive" ]
                [ span [ class "icon icon-play" ]
                    []
                ]
            , div [ class "btn-group" ]
                [ button [ class "btn btn-default" ]
                    [ span [ class "icon icon-doc-text" ]
                        []
                    ]
                , button [ class "btn btn-default" ]
                    [ span [ class "icon icon-folder" ] []
                    ]
                , button [ class "btn btn-default" ] [ span [ class "icon icon-floppy" ] [] ]
                ]
            , div
                [ class "btn-group" ]
                [ button [ class "btn btn-default active" ]
                    [ span [ class "icon icon-reply" ]
                        []
                    ]
                , button [ class "btn btn-default" ]
                    [ span [ class "icon icon-forward" ]
                        []
                    ]
                ]
            , button [ class "btn btn-default" ]
                [ span [ class "icon icon-tools" ]
                    []
                ]
            ]
        ]
