module WebTiled.Util.Tiled exposing
    ( dependencies
    , firstGid
    , getLevelData
    , images
    , sourceTileset
    , updateLevelData
    )

import Dict
import Set exposing (Set)
import Tiled.Layer as Layer
import Tiled.Level as Level exposing (Level, LevelData)
import Tiled.Tileset as Tileset exposing (Tileset)


sourceTileset : Level -> List Tileset.SourceTileData
sourceTileset level =
    let
        { layers, tilesets } =
            getLevelData level
    in
    tilesets
        |> List.foldl
            (\tileset acc ->
                case tileset of
                    Tileset.Source info ->
                        info :: acc

                    _ ->
                        acc
            )
            []


images : Level -> Set String
images level =
    let
        info =
            getLevelData level
    in
    info.tilesets
        |> List.foldl
            (\tileset acc ->
                case tileset of
                    Tileset.Embedded { image } ->
                        Set.insert image acc

                    Tileset.ImageCollection { tiles } ->
                        List.foldl (.image >> Set.insert) acc (Dict.values tiles)

                    _ ->
                        acc
            )
            Set.empty
        |> (\tilesets ->
                List.foldl
                    (\layer acc ->
                        case layer of
                            Layer.Image { image } ->
                                Set.insert image acc

                            _ ->
                                acc
                    )
                    tilesets
                    info.layers
           )


firstGid : Tileset -> Int
firstGid item =
    case item of
        Tileset.Source info ->
            info.firstgid

        Tileset.Embedded info ->
            info.firstgid

        Tileset.ImageCollection info ->
            info.firstgid


dependencies : Level -> Set String
dependencies level =
    let
        info =
            getLevelData level
    in
    info.tilesets
        |> List.foldl
            (\tileset acc ->
                case tileset of
                    Tileset.Embedded { image } ->
                        Set.insert image acc

                    Tileset.Source { source } ->
                        Set.insert source acc

                    _ ->
                        acc
            )
            Set.empty
        |> (\tilesets ->
                List.foldl
                    (\layer acc ->
                        case layer of
                            Layer.Image { image } ->
                                Set.insert image acc

                            _ ->
                                acc
                    )
                    tilesets
                    info.layers
           )


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
