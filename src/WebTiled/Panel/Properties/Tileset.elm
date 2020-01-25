module WebTiled.Panel.Properties.Tileset exposing (..)

import Tiled.Properties exposing (Property(..))
import WebTiled.Panel.Properties.Generic exposing (property, readOnly)


view tileset =
    [ property "Name" (String tileset.name)
    , property "Image" (File tileset.image)
    , readOnly "Image Size (w × h)" <| String.fromInt tileset.imagewidth ++ " × " ++ String.fromInt tileset.imageheight
    , property "Tile Width" (Int tileset.tilewidth)
    , property "Tile Height" (Int tileset.tileheight)
    , property "Margin" (Int tileset.margin)
    , property "Spacing" (Int tileset.spacing)
    , property "Transparent Color" (Color tileset.transparentcolor)
    , readOnly "Count" <| String.fromInt tileset.tilecount
    ]
