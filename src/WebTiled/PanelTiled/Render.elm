module WebTiled.PanelTiled.Render exposing (Model, init, view)

import Html
import Html.Attributes as Html
import Html.Lazy
import IDE.UI.Transform.DragScale as DragScale exposing (DragScale)
import Tiled.Level as Tiled
import Tiled.Util
import TypedSvg exposing (..)
import TypedSvg.Attributes exposing (viewBox)
import TypedSvg.Attributes.InPx exposing (..)
import TypedSvg.Events
import WebTiled.Svg.Level


type alias Model =
    DragScale {}


init : Model
init =
    { scale = 1
    , drag =
        { x = 0
        , y = 0
        }
    }


view : Model -> Tiled.Level -> Html.Html (Model -> Model)
view m l =
    let
        info =
            Tiled.Util.levelData l

        w =
            toFloat <| info.width * info.tilewidth

        h =
            toFloat <| info.height * info.tileheight
    in
    Html.div
        [ Html.style "background-color" info.backgroundcolor
        , Html.style "overflow" "hidden"
        , Html.style "height" "100%"
        , TypedSvg.Events.on "wheel" DragScale.handlerWheel
        ]
        [ Html.div
            [ Html.style "position" "absolute"
            , DragScale.apply m
            ]
            [ Html.Lazy.lazy5 content info.layers w h info.tilewidth info.tileheight ]
        ]


content layers_ w h tilewidth tileheight =
    svg
        [ width w
        , height h
        , viewBox 0 0 w h
        , Html.style "display" "block"
        ]
        [ WebTiled.Svg.Level.layers
            layers_
            (toFloat tilewidth)
            (toFloat tileheight)
        , WebTiled.Svg.Level.grid 5 (toFloat tilewidth) (toFloat tileheight)
        ]
