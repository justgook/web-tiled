module WebTiled.Util.Tiled exposing
    ( dependencies
    , firstGid
    , getLevelData
    , images
    , mergeTileset
    , sourceTileset
    , updateLevelData
    , validate
    )

import Dict exposing (Dict)
import Set exposing (Set)
import Tiled.Layer as Layer
import Tiled.Level as Level exposing (Level, LevelData)
import Tiled.Tileset as Tileset exposing (Tileset)


mergeTileset : Dict String Tileset -> Level -> Level
mergeTileset ts =
    updateLevelData
        (\l ->
            { l
                | tilesets =
                    List.foldl
                        (\t acc ->
                            case t of
                                Tileset.Source info ->
                                    Dict.get info.source ts
                                        |> Maybe.map (\a -> a :: acc)
                                        |> Maybe.withDefault (t :: acc)

                                _ ->
                                    t :: acc
                        )
                        []
                        l.tilesets
            }
        )


validate : Level -> Dict.Dict String v -> Dict.Dict String a -> Result (Set String) Level
validate level tilesets images_ =
    let
        missing =
            Dict.keys images_
                |> Set.fromList
                |> Set.diff (dependencies level)
                |> flip Set.diff (Set.fromList (Dict.keys tilesets))
    in
    if Set.isEmpty missing then
        Ok level

    else
        Err missing


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


flip : (c -> b -> a) -> b -> c -> a
flip fn a b =
    fn b a
