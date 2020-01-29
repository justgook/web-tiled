module WebTiled.Panel exposing
    ( fileManager
    , layers
    , preferences
    , preview
    , properties
    , render
    , statusbar
    , tilesets
    , toolbar
    , topMenu
    )

import Dict
import Html exposing (Attribute, Html, a, div, span, text)
import Html.Attributes exposing (style)
import Html.Lazy
import IDE.Internal.List as List
import IDE.UI.Layout as UI
import Tiled.Layer exposing (Layer(..))
import Tiled.Level exposing (Level)
import Tiled.Tileset as Tileset
import WebTiled.Message exposing (Message(..))
import WebTiled.Model exposing (CurrentLevel(..), Kind(..), LevelFrom(..), Model, PropertiesFor(..))
import WebTiled.Panel.Generic
import WebTiled.Panel.Layers
import WebTiled.Panel.Preferences
import WebTiled.Panel.Preview
import WebTiled.Panel.Progress
import WebTiled.Panel.Properties.Generic
import WebTiled.Panel.Properties.Layer
import WebTiled.Panel.Properties.Map
import WebTiled.Panel.Properties.Tileset
import WebTiled.Panel.Statusbar
import WebTiled.Panel.Tilesets
import WebTiled.Panel.Toolbar.RemoteStorage
import WebTiled.Panel.Toolbar.Run
import WebTiled.Panel.Toolbar.TileLayer
import WebTiled.Panel.Toolbar.Tools
import WebTiled.Panel.TopMenu
import WebTiled.Util.Tiled as TiledUtil


preview : UI.Layout Kind
preview =
    UI.node Preview |> UI.setLimits { yMax = Nothing, yMin = 10, xMax = Nothing, xMin = 10 }


fileManager : UI.Layout Kind
fileManager =
    UI.node FileManager


preferences : UI.Layout Kind
preferences =
    UI.node Preferences


tilesets : UI.Layout Kind
tilesets =
    UI.node Tilesets


topMenu : UI.Layout Kind
topMenu =
    let
        default =
            { xMin = 10
            , xMax = Nothing
            , yMin = 10
            , yMax = Nothing
            }
    in
    UI.node TopMenu |> UI.setLimits { default | yMin = 22, yMax = Just 22 }


toolbar : UI.Layout Kind
toolbar =
    ( UI.node RunTools |> UI.setLimits { yMax = Just 34, yMin = 34, xMax = Just 120, xMin = 120 }
    , [ UI.node MainTools |> UI.setLimits { yMax = Just 34, yMin = 34, xMax = Just 220, xMin = 220 }
      , UI.node TileLayerTools |> UI.setLimits { yMax = Just 34, yMin = 34, xMax = Just 206, xMin = 206 }
      , UI.node CloudTools |> UI.setLimits { yMax = Just 34, yMin = 34, xMax = Nothing, xMin = 40 }
      ]
    )
        |> UI.fromList


statusbar : UI.Layout Kind
statusbar =
    ( UI.node Statusbar |> UI.setLimits { yMax = Just 26, yMin = 26, xMax = Nothing, xMin = 20 }, [] )
        |> UI.fromList


layers : UI.Layout Kind
layers =
    UI.node Layers
        |> UI.setLimits { yMax = Nothing, yMin = 20, xMax = Just 300, xMin = 20 }


properties : UI.Layout Kind
properties =
    UI.node Properties
        |> UI.setLimits { yMax = Nothing, yMin = 20, xMax = Just 300, xMin = 20 }


render : Model -> Int -> Int -> Int -> Int -> Kind -> Html Message
render model w h x y kind =
    case ( kind, model.level ) of
        ( FakeProgress, _ ) ->
            WebTiled.Panel.Progress.view

        ( TopMenu, _ ) ->
            WebTiled.Panel.TopMenu.view

        ( Preview, LevelComplete _ level _ ) ->
            [ WebTiled.Panel.Preview.view (TiledUtil.getLevelData level) ]
                |> wrapper x y w h

        ( Layers, LevelComplete _ level _ ) ->
            WebTiled.Panel.Layers.view model.selectedLayers (TiledUtil.getLevelData level).layers
                |> WebTiled.Panel.Generic.panel x y w h "Layers"

        ( Layers, _ ) ->
            WebTiled.Panel.Layers.view model.selectedLayers []
                |> WebTiled.Panel.Generic.panel x y w h "Layers"

        ( Preferences, _ ) ->
            WebTiled.Panel.Preferences.view model.settings

        ( RunTools, _ ) ->
            [ WebTiled.Panel.Toolbar.Run.view model.build ]
                |> wrapper x y w h

        ( MainTools, _ ) ->
            [ WebTiled.Panel.Toolbar.Tools.view ]
                |> wrapper x y w h

        ( TileLayerTools, _ ) ->
            [ WebTiled.Panel.Toolbar.TileLayer.view ]
                |> wrapper x y w h

        ( CloudTools, _ ) ->
            [ WebTiled.Panel.Toolbar.RemoteStorage.view ]
                |> wrapper x y w h

        ( Statusbar, _ ) ->
            [ WebTiled.Panel.Statusbar.view model.version ]
                |> wrapper x y w h

        ( FileManager, _ ) ->
            div [] [ text "Loading.." ] |> WebTiled.Panel.Generic.panel x y w h "File Manager"

        ( Tilesets, _ ) ->
            case model.level of
                LevelComplete (UrlLevel url) level external ->
                    [ Html.Lazy.lazy3 WebTiled.Panel.Tilesets.imagesFromUrl url (TiledUtil.getLevelData level).tilesets external
                    , Html.Lazy.lazy3 WebTiled.Panel.Tilesets.view model.selectedTileset (TiledUtil.getLevelData level).tilesets external
                        |> WebTiled.Panel.Generic.panel x y w h "Tilesets"
                    ]
                        |> div []

                LevelComplete (DiskLevel images) level external ->
                    [ Html.Lazy.lazy2 WebTiled.Panel.Tilesets.imagesFromDisk images (TiledUtil.getLevelData level).tilesets
                    , Html.Lazy.lazy3 WebTiled.Panel.Tilesets.view model.selectedTileset (TiledUtil.getLevelData level).tilesets external
                        |> WebTiled.Panel.Generic.panel x y w h "Tilesets"
                    ]
                        |> div []

                LevelComplete (RemoteStorageLevel _) _ _ ->
                    div [] [] |> WebTiled.Panel.Generic.panel x y w h "Tilesets"

                LevelLoading _ _ _ _ ->
                    div [] [ text "Loading... " ] |> WebTiled.Panel.Generic.panel x y w h "Tilesets"

                LevelPartial _ _ _ _ ->
                    div [] [ text "Missing files" ] |> WebTiled.Panel.Generic.panel x y w h "Tilesets"

                LevelNone ->
                    div [] [ text "No Level" ] |> WebTiled.Panel.Generic.panel x y w h "Tilesets"

        ( Properties, LevelComplete _ level external ) ->
            case model.propertiesFocus of
                TilesetProps i ->
                    case (TiledUtil.getLevelData level).tilesets |> List.getAt i of
                        Just (Tileset.Source { source, firstgid }) ->
                            case Dict.get firstgid external of
                                Just (Tileset.Embedded info) ->
                                    WebTiled.Panel.Properties.Tileset.view info
                                        |> WebTiled.Panel.Properties.Generic.properties "Tileset" info.properties x y w h

                                _ ->
                                    span [] [ text ("Loading (" ++ source ++ ")") ]
                                        |> WebTiled.Panel.Generic.panel x y w h "Tilesets"

                        Just (Tileset.Embedded info) ->
                            WebTiled.Panel.Properties.Tileset.view info
                                |> WebTiled.Panel.Properties.Generic.properties "Tileset" info.properties x y w h

                        Just (Tileset.ImageCollection info) ->
                            span [] [ text "NOT IMPLEMENTED YET" ]
                                |> WebTiled.Panel.Generic.panel x y w h "Tilesets"

                        Nothing ->
                            span [] [ text "Tileset Deleted" ]
                                |> WebTiled.Panel.Generic.panel x y w h "Tilesets"

                TileProps _ ->
                    span [] [ text "TileProps" ]

                LayerProps i ->
                    case (TiledUtil.getLevelData level).layers |> List.getAt i of
                        Just (Tile info) ->
                            WebTiled.Panel.Properties.Layer.view
                                { name = info.name
                                , opacity = info.opacity
                                , visible = info.visible
                                , x = info.x
                                , y = info.y
                                }
                                |> WebTiled.Panel.Properties.Generic.properties "Tile Layer" info.properties x y w h

                        Just (Object info) ->
                            WebTiled.Panel.Properties.Layer.view
                                { name = info.name
                                , opacity = info.opacity
                                , visible = info.visible
                                , x = info.x
                                , y = info.y
                                }
                                |> WebTiled.Panel.Properties.Generic.properties "Object Layer" info.properties x y w h

                        Just (Image info) ->
                            WebTiled.Panel.Properties.Layer.view
                                { name = info.name
                                , opacity = info.opacity
                                , visible = info.visible
                                , x = info.x
                                , y = info.y
                                }
                                |> WebTiled.Panel.Properties.Generic.properties "Image Layer" info.properties x y w h

                        Just (InfiniteTile info) ->
                            WebTiled.Panel.Properties.Layer.view
                                { name = info.name
                                , opacity = info.opacity
                                , visible = info.visible
                                , x = info.x
                                , y = info.y
                                }
                                |> WebTiled.Panel.Properties.Generic.properties "Infinite Tile Layer" info.properties x y w h

                        Nothing ->
                            span [] [ text "Layer deleted" ]
                                |> WebTiled.Panel.Generic.panel x y w h "Properties"

                ObjectProps _ ->
                    span [] [ text "ObjectProps" ]

                LevelProps ->
                    let
                        info =
                            TiledUtil.getLevelData level
                    in
                    WebTiled.Panel.Properties.Map.view
                        { backgroundcolor = info.backgroundcolor
                        , height = info.height
                        , infinite = info.infinite
                        , renderorder = info.renderorder
                        , tiledversion = info.tiledversion
                        , tileheight = info.tileheight
                        , tilewidth = info.tilewidth
                        , version = info.version
                        , width = info.width
                        }
                        |> WebTiled.Panel.Properties.Generic.properties "Map" info.properties x y w h

        _ ->
            span [] [ text "NOT IMPLEMENTED YET" ]


wrapper x y w h =
    let
        px : Int -> String
        px i =
            String.fromInt i ++ "px"
    in
    div
        [ style "width" <| px w
        , style "height" <| px h
        , style "top" <| px y
        , style "left" <| px x
        , style "position" "absolute"
        ]
