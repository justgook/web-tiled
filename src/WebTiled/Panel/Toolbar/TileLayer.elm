module WebTiled.Panel.Toolbar.TileLayer exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)


view =
    header [ class "toolbar toolbar-header" ]
        [ div [ class "toolbar-actions" ]
            [ div [ class "btn-group" ]
                [ button [ class "btn btn-default" ] [ span [ class "icon flaticon-rubber-stamp" ] [] ]
                , button [ class "btn btn-default" ] [ span [ class "icon flaticon-paint-bucket-2" ] [] ]
                , button [ class "btn btn-default" ] [ span [ class "icon flaticon-cropping-tool-point-1" ] [] ]
                , button [ class "btn btn-default" ] [ span [ class "icon flaticon-eraser" ] [] ]
                , button [ class "btn btn-default" ] [ span [ class "icon flaticon-dotted-square" ] [] ]
                , button [ class "btn btn-default" ] [ span [ class "icon flaticon-magic-wand-with-a-star" ] [] ]
                ]
            ]
        ]
