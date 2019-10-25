module Tiled.Util exposing (getLevelData, updateLevelData)

import Tiled.Level as Level exposing (Level, LevelData)


updateLevelData : (LevelData -> LevelData) -> Level -> Level
updateLevelData fn level =
    case level of
        Level.Orthogonal info ->
            Level.Orthogonal (fn info)

        Level.Isometric info ->
            Level.Isometric (fn info)

        --        Level.Staggered info ->
        --            Level.Staggered
        --                fn
        --                { backgroundcolor = info.backgroundcolor
        --                , height = info.height
        --                , infinite = info.infinite
        --                , layers = info.layers
        --                , nextobjectid = info.nextobjectid
        --                , renderorder = info.renderorder
        --                , tiledversion = info.tiledversion
        --                , tileheight = info.tileheight
        --                , tilesets = info.tilesets
        --                , tilewidth = info.tilewidth
        --                , version = info.version
        --                , width = info.width
        --                , properties = info.properties
        --                }
        _ ->
            level



--        Level.Hexagonal info ->
--            Level.Hexagonal
--                { backgroundcolor = info.backgroundcolor
--                , height = info.height
--                , infinite = info.infinite
--                , layers = info.layers
--                , nextobjectid = info.nextobjectid
--                , renderorder = info.renderorder
--                , tiledversion = info.tiledversion
--                , tileheight = info.tileheight
--                , tilesets = info.tilesets
--                , tilewidth = info.tilewidth
--                , version = info.version
--                , width = info.width
--                , properties = info.properties
--                }


getLevelData : Level -> LevelData
getLevelData level =
    case level of
        Level.Orthogonal info ->
            { backgroundcolor = info.backgroundcolor
            , height = info.height
            , infinite = info.infinite
            , layers = info.layers
            , nextobjectid = info.nextobjectid
            , renderorder = info.renderorder
            , tiledversion = info.tiledversion
            , tileheight = info.tileheight
            , tilesets = info.tilesets
            , tilewidth = info.tilewidth
            , version = info.version
            , width = info.width
            , properties = info.properties
            }

        Level.Isometric info ->
            { backgroundcolor = info.backgroundcolor
            , height = info.height
            , infinite = info.infinite
            , layers = info.layers
            , nextobjectid = info.nextobjectid
            , renderorder = info.renderorder
            , tiledversion = info.tiledversion
            , tileheight = info.tileheight
            , tilesets = info.tilesets
            , tilewidth = info.tilewidth
            , version = info.version
            , width = info.width
            , properties = info.properties
            }

        Level.Staggered info ->
            { backgroundcolor = info.backgroundcolor
            , height = info.height
            , infinite = info.infinite
            , layers = info.layers
            , nextobjectid = info.nextobjectid
            , renderorder = info.renderorder
            , tiledversion = info.tiledversion
            , tileheight = info.tileheight
            , tilesets = info.tilesets
            , tilewidth = info.tilewidth
            , version = info.version
            , width = info.width
            , properties = info.properties
            }

        Level.Hexagonal info ->
            { backgroundcolor = info.backgroundcolor
            , height = info.height
            , infinite = info.infinite
            , layers = info.layers
            , nextobjectid = info.nextobjectid
            , renderorder = info.renderorder
            , tiledversion = info.tiledversion
            , tileheight = info.tileheight
            , tilesets = info.tilesets
            , tilewidth = info.tilewidth
            , version = info.version
            , width = info.width
            , properties = info.properties
            }
