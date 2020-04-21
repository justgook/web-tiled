module WebTiled.Panel.Preview exposing (content, view)

import Html exposing (div)
import Html.Attributes exposing (style)
import Html.Lazy as Lazy
import IDE.UI.Transform.DragScale as DragScale
import Tiled.Layer exposing (Layer)
import TypedSvg as Svg
import TypedSvg.Attributes
import TypedSvg.Attributes.InPx as SvgInPx
import WebTiled.Svg.Level


view :
    { a
        | width : Int
        , tilewidth : Int
        , height : Int
        , tileheight : Int
        , backgroundcolor : String
        , layers : List Layer
    }
    -> Html.Html msg
view info =
    let
        w =
            toFloat <| info.width * info.tilewidth

        h =
            toFloat <| info.height * info.tileheight

        m =
            { drag = { x = 20, y = 20 }, scale = 1 }
    in
    div
        [ style "background-color" info.backgroundcolor
        , style "overflow" "hidden"
        , style "width" "100%"
        , style "height" "100%"
        , style "overflow" "hidden"

        --, TypedSvg.Events.on "wheel" DragScale.handlerWheel
        ]
        [ div
            [ style "display" "table"
            ]
            [ div [ DragScale.apply m ]
                [ Lazy.lazy5 content info.layers w h info.tilewidth info.tileheight

                --, Select.select m
                ]
            ]
        ]


content : List Layer -> Float -> Float -> Int -> Int -> Html.Html msg
content layers_ w h tilewidth tileheight =
    Svg.svg
        [ SvgInPx.width w
        , SvgInPx.height h
        , TypedSvg.Attributes.viewBox 0 0 w h
        , style "display" "block"
        ]
        [ WebTiled.Svg.Level.layers
            layers_
            (toFloat tilewidth)
            (toFloat tileheight)
        , WebTiled.Svg.Level.grid 5 (toFloat tilewidth) (toFloat tileheight)
        ]
