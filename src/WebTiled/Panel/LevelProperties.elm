module WebTiled.Panel.LevelProperties exposing (view)

import Dict
import Html exposing (td, text, tr)
import IDE.Internal.Cursor as Cursor
import IDE.UI.Widget as Widget
import Tiled.Level as Tiled
import Tiled.Properties exposing (Properties, Property(..))
import Tiled.Util
import WebTiled.Panel.Properties exposing (customProps, propertiesTable, propertyRow)


view widgetCache level =
    propertiesTable ( "Map", levelProperties widgetCache level )
        ((Tiled.Util.getLevelData level).properties
            |> customProps
        )


levelProperties widgetCache level =
    let
        info =
            Tiled.Util.getLevelData level

        widthSet value =
            Tiled.Util.updateLevelData (\level_ -> { level_ | width = value })

        heightSet value =
            Tiled.Util.updateLevelData (\level_ -> { level_ | height = value })

        tileWidthSet value =
            Tiled.Util.updateLevelData (\level_ -> { level_ | tilewidth = value })

        tileHeightSet value =
            Tiled.Util.updateLevelData (\level_ -> { level_ | tileheight = value })
    in
    [ propertyRow "Orientation" (PropString "Todo")
    , propertyInt "Width" widthSet "level.width" widgetCache info.width
    , propertyInt "Height" heightSet "level.height" widgetCache info.height
    , propertyInt "Tile Width" tileWidthSet "level.tilewidth" widgetCache info.tilewidth
    , propertyInt "Tile Height" tileHeightSet "level.tileheight" widgetCache info.tileheight

    --    , propertyRow "Tile Width" (PropInt info.tilewidth)
    --    , propertyRow "Tile Height" (PropInt info.tileheight)
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


propertyInt name set key cache value =
    let
        m =
            Dict.get key cache.number
                |> Maybe.map (\string -> { value = value, string = string })
                |> Maybe.withDefault { value = value, string = String.fromInt value }
    in
    tr []
        [ td []
            [ text name ]
        , td []
            [ Widget.int m ]
        ]
        |> Html.map (mapper set key value)


mapper set key v fn editor level =
    let
        m =
            Dict.get key Dict.empty
                |> Maybe.map (\s -> { value = v, string = s })
                |> Maybe.withDefault { value = v, string = String.fromInt v }

        { value, string } =
            fn m

        widgetCache =
            editor.widgetCache
    in
    ( { editor
        | widgetCache =
            { widgetCache
                | number = Dict.insert key string widgetCache.number
            }
      }
    , set value level
    )
