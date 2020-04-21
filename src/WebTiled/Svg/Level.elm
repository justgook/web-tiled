module WebTiled.Svg.Level exposing (grid, layers, view)

import Color
import Html.Lazy
import IDE.Internal.List as List
import Tiled.Layer as Layer
import Tiled.Object exposing (Object(..))
import TypedSvg exposing (..)
import TypedSvg.Attributes exposing (d, noFill, patternUnits, stroke, strokeDasharray, viewBox, xlinkHref)
import TypedSvg.Attributes.InPx exposing (..)
import TypedSvg.Core exposing (Svg, attribute)
import TypedSvg.Types exposing (CoordinateSystem(..), Paint(..), percent)


view : List Layer.Layer -> Float -> Float -> Float -> Float -> Svg msg
view layers_ w h tilewidth tileheight =
    --        , Html.Lazy.lazy3 grid 5 (toFloat info.tilewidth) (toFloat info.tileheight)
    svg [ width w, height h, viewBox 0 0 w h ] [ layers layers_ tilewidth tileheight ]


layers : List Layer.Layer -> Float -> Float -> Svg msg
layers layerList tilewidth tileheight =
    List.map (Html.Lazy.lazy3 layer tilewidth tileheight) layerList |> g []


layer : Float -> Float -> Layer.Layer -> Svg msg
layer tilewidth tileheight l =
    case l of
        Layer.Image imageData ->
            g [] []

        Layer.Object info ->
            layerObject info
                |> g []

        Layer.Tile tileData ->
            layerTile tilewidth tileheight tileData
                |> g []

        Layer.InfiniteTile tileChunkedData ->
            g [] []


layerObject { objects } =
    List.map
        (\object ->
            case object of
                Point info ->
                    g [] []

                Rectangle info ->
                    g [] []

                Ellipse info ->
                    g [] []

                Polygon info ->
                    g [] []

                PolyLine info ->
                    g [] []

                Tile { gid, x, y } ->
                    tile x y gid
        )
        objects


layerTile : Float -> Float -> { a | data : List Int, width : Int } -> List (Svg msg)
layerTile tilewidth tileheight info =
    info.data
        |> List.indexedFoldl
            (\i gid acc ->
                if gid == 0 then
                    acc

                else
                    let
                        x_ =
                            toFloat (remainderBy info.width i)
                                |> (*) tilewidth

                        y_ =
                            toFloat (i // info.width)
                                |> (*) tileheight
                    in
                    tile x_ y_ gid :: acc
            )
            []


tile : Float -> Float -> Int -> Svg msg
tile x_ y_ gid =
    use [ xlinkHref <| "#" ++ String.fromInt gid, x x_, y y_ ] []


grid : Float -> Float -> Float -> Svg msg
grid group cellW cellH =
    Html.Lazy.lazy3 grid_ group cellW cellH


grid_ group cellW cellH =
    g []
        [ gridDefs group cellW cellH
        , rect
            [ TypedSvg.Attributes.width <| percent 100
            , TypedSvg.Attributes.height <| percent 100
            , attribute "fill" "url(#grid)"
            , stroke (Paint Color.black)
            , strokeWidth 2
            ]
            []
        ]


gridDefs : Float -> Float -> Float -> Svg msg
gridDefs group cellW cellH =
    defs []
        [ pattern
            [ width cellW
            , height cellH
            , attribute "id" "smallGrid"
            , patternUnits CoordinateSystemUserSpaceOnUse
            ]
            [ path
                [ d ("M " ++ String.fromFloat cellW ++ " 0 L 0 0 0 " ++ String.fromFloat cellW)
                , noFill
                , stroke (Paint (Color.rgba 0.82 0.84 0.81 0.3))
                , strokeWidth 1
                , strokeDasharray "2 1"
                ]
                []
            ]
        , pattern
            [ width (group * cellW)
            , height (group * cellH)
            , attribute "id" "grid"
            , patternUnits CoordinateSystemUserSpaceOnUse
            ]
            [ rect
                [ attribute "fill" "url(#smallGrid)"
                , width (group * cellW)
                , height (group * cellH)
                ]
                []
            , path
                [ d ("M " ++ String.fromFloat (group * cellW) ++ " 0 L 0 0 0 " ++ String.fromFloat (group * cellH))
                , noFill
                , stroke (Paint (Color.rgba 0.82 0.84 0.81 0.3))
                , strokeWidth 2
                ]
                []
            ]
        ]
