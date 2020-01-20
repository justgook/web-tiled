module WebTiled.Tutorial exposing (..)

--https://github.com/lyngbach/uxTour

import Html exposing (div)
import Html.Attributes exposing (style)


type alias Model =
    ()


init : Model
init =
    ()


view =
    div
        [ style "position" "absolute"
        , style "z-index" "999991"
        , style "border-radius" "50%"
        , style "box-shadow" "rgba(0, 0, 0, 0.7) 0px 0px 0px 9999px"
        , style "top" "194px"
        , style "left" "242.5px"
        ]
        []
