module WebTiled.Panel.Statusbar exposing (view)

import Html exposing (..)
import Html.Attributes exposing (class, style)


view : String -> Html msg
view version =
    footer [ class "toolbar toolbar-footer padded-horizontally-less" ]
        [ div [ style "float" "right" ]
            [ button [ class "btn-mini" ] [ span [ class "icon icon-gauge" ] [] ]
            , text <| " V:" ++ version
            ]
        ]
