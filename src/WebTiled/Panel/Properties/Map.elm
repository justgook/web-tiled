module WebTiled.Panel.Properties.Map exposing (view)

import Html exposing (Html)
import Tiled.Level exposing (RenderOrder(..))
import Tiled.Properties exposing (Property(..))
import WebTiled.Panel.Properties.Generic exposing (property, readOnly)


view : { a | backgroundcolor : String, height : Int, infinite : Bool, renderorder : RenderOrder, tiledversion : String, tileheight : Int, tilewidth : Int, version : Float, width : Int } -> List (Html msg)
view { backgroundcolor, height, infinite, renderorder, tiledversion, tileheight, tilewidth, version, width } =
    [ property "Orientation" (String "Orthogonal")
    , property "Width" (Int width)
    , property "Height" (Int height)
    , property "infinite" (Bool infinite)
    , property "Render Order" (String (orderToString renderorder))
    , property "Tile Height" (Int tileheight)
    , property "Tile Width" (Int tilewidth)
    , property "Background Color" (Color backgroundcolor)
    , readOnly "Version" (String.fromFloat version)
    , readOnly "Sub Version" tiledversion
    ]


orderToString : RenderOrder -> String
orderToString a =
    case a of
        RightDown ->
            "Right Down"

        RightUp ->
            "Right Up"

        LeftDown ->
            "Left Down"

        LeftUp ->
            "Left Up"
