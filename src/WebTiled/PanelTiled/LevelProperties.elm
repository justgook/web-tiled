module WebTiled.PanelTiled.LevelProperties exposing (view)

import IDE.Internal.Cursor as Cursor
import Tiled.Level as Tiled
import Tiled.Properties exposing (Properties, Property(..))
import Tiled.Util
import WebTiled.PanelTiled.Properties exposing (customProps, propertiesTable, propertyRow)


view level =
    propertiesTable ( "Map", levelProperties level )
        ((Tiled.Util.levelData level).properties
            |> customProps
        )


levelProperties l =
    let
        info =
            Tiled.Util.levelData l

        _ =
            Cursor.update
                { get = .width
                , set = \a b -> { a | width = b }
                }

        _ =
            info.width
                |> Debug.log "hello22"
    in
    [ propertyRow "Orientation" (PropString "Todo")
    , propertyRow "Width" (PropInt info.width)
    , propertyRow "Height" (PropInt info.height)
    , propertyRow "Tile Width" (PropInt info.tilewidth)
    , propertyRow "Tile Height" (PropInt info.tileheight)
    , propertyRow "Infinite" (PropBool info.infinite)

    --    , propertyRow "Tile Side Length (HEX)" (PropString "Todo")
    --    , propertyRow "Stagger Axis" (PropString "Todo")
    --    , propertyRow "Stagger Index" (PropString "Todo")
    --    , propertyRow "Tile Layer Format" (PropString "Todo")
    --    , propertyRow "Output Chunk Width" (PropString "Todo")
    --    , propertyRow "Output Chunk Height" (PropString "Todo")
    , propertyRow "Tile Render Order" (PropString "Todo")

    --    , propertyRow "Compression Level" (PropString "Todo")
    , propertyRow "Background Color" (PropColor info.backgroundcolor)
    ]
