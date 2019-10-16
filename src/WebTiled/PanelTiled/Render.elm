module WebTiled.PanelTiled.Render exposing (view)

import Color
import Html
import Html.Attributes as Html
import Html.Lazy
import Tiled.Layer as Layer
import Tiled.Level as Tiled
import Tiled.Util
import TypedSvg exposing (..)
import TypedSvg.Attributes exposing (d, fill, noFill, patternUnits, stroke, strokeDasharray, viewBox, xlinkHref)
import TypedSvg.Attributes.InPx exposing (..)
import TypedSvg.Core exposing (Svg, attribute)
import TypedSvg.Types exposing (CoordinateSystem(..), percent)
import WebTiled.Internal.List as List


view : Tiled.Level -> Html.Html msg
view l =
    let
        info =
            Tiled.Util.levelData l

        w =
            toFloat <| info.width * info.tilewidth

        h =
            toFloat <| info.height * info.tileheight
    in
    Html.div
        [ Html.style "min-width" ("calc(100% + " ++ String.fromFloat (w * 0.5) ++ "px" ++ ")")
        , Html.style "min-height" ("calc(100% + " ++ String.fromFloat (h * 0.5) ++ "px" ++ ")")
        , Html.style "width" (String.fromFloat (w * 1.25) ++ "px")
        , Html.style "height" (String.fromFloat (h * 1.25) ++ "px")
        , Html.style "display" "flex"
        , Html.style "align-items" "center"
        , Html.style "justify-content" "center"
        , Html.style "background-color" info.backgroundcolor
        ]
        [ svg
            [ width w
            , height h
            , viewBox 0 0 w h
            ]
            [ layers
                info.layers
                (toFloat info.tilewidth)
                (toFloat info.tileheight)
            , Html.Lazy.lazy3 grid 5 (toFloat info.tilewidth) (toFloat info.tileheight)
            ]
        ]


layers : List Layer.Layer -> Float -> Float -> Svg msg
layers a tilewidth tileheight =
    List.map (Html.Lazy.lazy3 layer tilewidth tileheight) a |> g []


layer : Float -> Float -> Layer.Layer -> Svg msg
layer tilewidth tileheight l =
    case l of
        Layer.Image imageData ->
            g [] []

        Layer.Object objectData ->
            g [] []

        Layer.Tile tileData ->
            layerTile tilewidth tileheight tileData
                |> g []

        Layer.InfiniteTile tileChunkedData ->
            g [] []


layerTile : Float -> Float -> { a | data : List Int, width : Int } -> List (Svg msg)
layerTile tilewidth tileheight info =
    info.data
        |> List.indexedFoldl
            (\i gid acc ->
                if gid == 0 then
                    acc

                else
                    tile info.width tilewidth tileheight i gid :: acc
            )
            []


tile : Int -> Float -> Float -> Int -> Int -> Svg msg
tile columns cellWidth cellHeight i gid =
    let
        column =
            toFloat (remainderBy columns i)

        row =
            toFloat (i // columns)

        x_ =
            cellWidth * column

        y_ =
            cellHeight * row
    in
    use
        [ xlinkHref <| "#" ++ String.fromInt gid
        , x x_
        , y y_
        ]
        []


grid : Float -> Float -> Float -> Svg msg
grid group cellW cellH =
    g []
        [ gridDefs group cellW cellH
        , rect
            [ TypedSvg.Attributes.width <| percent 100
            , TypedSvg.Attributes.height <| percent 100
            , attribute "fill" "url(#grid)"
            , stroke Color.black
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
                , stroke Color.gray
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
                , stroke Color.darkGray
                , strokeWidth 2
                ]
                []
            ]
        ]
