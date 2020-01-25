module WebTiled.Panel.Properties.Layer exposing (view)

import Tiled.Properties exposing (Property(..))
import WebTiled.Panel.Properties.Generic exposing (property)


view { name, opacity, visible, x, y } =
    [ property "Name" (String name)
    , property "Visible" (Bool visible)
    , property "Opacity" (Float opacity)
    , property "Horizontal Offset" (Float x)
    , property "Vertical Offset" (Float y)
    ]
