module WebTiled.PanelTiled exposing (Kind(..), Message(..), Model, block, init, view)

import Dict exposing (Dict)
import Html exposing (Attribute, Html, a, button, div, h1, header, nav, span, text)
import Html.Attributes exposing (class, style)
import IDE.UI2.Tree
import Tiled.Layer as Layer
import Tiled.Level as Tiled
import Tiled.Util
import WebTiled.PanelTiled.FileManager as FileManager
import WebTiled.PanelTiled.LevelProperties as LevelPropertiesPanel
import WebTiled.PanelTiled.Properties exposing (propertiesTable)
import WebTiled.PanelTiled.Render as RenderPanel
import WebTiled.PanelTiled.Tileset as TilesetPanel


type Kind
    = MainTools
    | LayerTools
    | ObjectTools
    | Properties
    | LevelProperties
    | Layers
    | Tilesets
    | Render
    | FileManager


block =
    let
        default =
            { xMin = 11
            , xMax = Nothing
            , yMin = 12
            , yMax = Nothing
            }
    in
    { mainTools =
        IDE.UI2.Tree.node MainTools
            |> IDE.UI2.Tree.setLimits
                { yMax = Just 34
                , yMin = 34
                , xMax = Just 222
                , xMin = 220
                }
    , layerTools =
        IDE.UI2.Tree.node LayerTools
            |> IDE.UI2.Tree.setLimits
                { yMax = Just 34
                , yMin = 34
                , xMax = Nothing
                , xMin = 206
                }
    , objectTools = IDE.UI2.Tree.node ObjectTools |> IDE.UI2.Tree.setLimits { default | xMax = Just 250 }
    , properties = IDE.UI2.Tree.node Properties |> IDE.UI2.Tree.setLimits { default | xMax = Just 250 }
    , levelProperties = IDE.UI2.Tree.node LevelProperties |> IDE.UI2.Tree.setLimits { default | xMax = Just 250 }
    , layers = IDE.UI2.Tree.node Layers |> IDE.UI2.Tree.setLimits { default | xMax = Just 250 }
    , tilesets = IDE.UI2.Tree.node Tilesets |> IDE.UI2.Tree.setLimits { default | xMax = Just 250 }
    , render = IDE.UI2.Tree.node Render |> IDE.UI2.Tree.setLimits default
    , fileManager = IDE.UI2.Tree.node FileManager |> IDE.UI2.Tree.setLimits default
    }


type Message msg
    = Editor (Model -> Model)
    | Level (Tiled.Level -> Tiled.Level)
    | EditorLevel (Model -> Tiled.Level -> ( Model, Tiled.Level ))
    | EditorCmd (Model -> ( Model, Cmd msg ))


type alias Model =
    { render : RenderPanel.Model
    , tilesets : TilesetPanel.Model
    , fileManager : FileManager.Model
    , widgetCache :
        { number : Dict String String
        }
    }


init : Model
init =
    { render = RenderPanel.init
    , tilesets = TilesetPanel.init
    , fileManager = FileManager.init
    , widgetCache =
        { number = Dict.empty
        }
    }


view { editor, relUrl, files, inStore } level w_ h_ kind =
    case kind of
        MainTools ->
            bare w_ h_ [ mainToolbar ]
                |> Html.map Editor

        LayerTools ->
            bare w_ h_ [ layerToolbar ]
                |> Html.map Editor

        ObjectTools ->
            bare w_ h_ []
                |> Html.map Editor

        LevelProperties ->
            LevelPropertiesPanel.view editor.widgetCache level
                |> panel w_ h_ "Properties"
                |> Html.map EditorLevel

        Properties ->
            panel w_ h_ "Properties" (propertiesTable ( "Object", [] ) [])
                |> Html.map Editor

        Layers ->
            bare w_ h_ (layers (Tiled.Util.getLevelData level).layers)
                |> Html.map Editor

        Tilesets ->
            [ TilesetPanel.view editor.tilesets relUrl files (Tiled.Util.getLevelData level).tilesets
                |> Html.map (\fn model -> { model | tilesets = fn model.tilesets })
            ]
                |> panel w_ h_ "Tilesets"
                |> Html.map Editor

        Render ->
            [ RenderPanel.view editor.render level
                |> Html.map (\fn model -> { model | render = fn model.render })
            ]
                |> panel w_ h_ "Render"
                |> Html.map Editor

        FileManager ->
            [ FileManager.view editor.fileManager files inStore
                |> Html.map
                    (\fn model ->
                        let
                            ( newModel, cmd ) =
                                fn model.fileManager
                        in
                        ( { model | fileManager = newModel }, cmd )
                    )
            ]
                |> panel w_ h_ "File Manager"
                |> Html.map EditorCmd


panel w_ h_ title content =
    bare w_
        h_
        [ header [ class "toolbar toolbar-header" ]
            [ h1 [ class "title" ]
                [ text title ]
            ]
        , div
            [ class "pane"
            , style "background" "#BFBFBF"
            , style "display" "flex"
            , style "flex-flow" "column"
            ]
            content
        ]


bare w_ h_ =
    let
        setWidth w =
            String.fromInt w ++ "px" |> style "width"

        setHeight w =
            String.fromInt w ++ "px" |> style "height"
    in
    div
        [ setWidth w_
        , setHeight h_
        , style "display" "flex"
        , style "flex-flow" "column"
        , style "position" "relative"
        ]


mainToolbar =
    header [ class "toolbar toolbar-header" ]
        [ div [ class "toolbar-actions" ]
            [ div [ class "btn-group" ]
                [ button [ class "btn btn-default" ]
                    [ span [ class "icon icon-doc-text" ]
                        []
                    ]
                , button [ class "btn btn-default" ]
                    [ span [ class "icon icon-folder" ] []
                    ]
                , button [ class "btn btn-default" ] [ span [ class "icon icon-floppy" ] [] ]
                ]
            , div
                [ class "btn-group" ]
                [ button [ class "btn btn-default active" ]
                    [ span [ class "icon icon-reply" ]
                        []
                    ]
                , button [ class "btn btn-default" ]
                    [ span [ class "icon icon-forward" ]
                        []
                    ]
                ]
            , button [ class "btn btn-default" ]
                [ span [ class "icon icon-tools" ]
                    []
                ]
            ]
        ]


layerToolbar =
    header [ class "toolbar toolbar-header" ]
        [ div [ class "toolbar-actions" ]
            [ div [ class "btn-group" ]
                [ button [ class "btn btn-default" ] [ span [ class "icon flaticon-rubber-stamp" ] [] ]
                , button [ class "btn btn-default" ] [ span [ class "icon flaticon-paint-bucket-2" ] [] ]
                , button [ class "btn btn-default" ] [ span [ class "icon flaticon-cropping-tool-point-1" ] [] ]
                , button [ class "btn btn-default" ] [ span [ class "icon flaticon-eraser" ] [] ]
                , button [ class "btn btn-default" ] [ span [ class "icon flaticon-dotted-square" ] [] ]
                , button [ class "btn btn-default" ] [ span [ class "icon flaticon-magic-wand-with-a-star" ] [] ]
                ]
            ]
        ]


layers l =
    [ header [ class "toolbar toolbar-header" ]
        [ h1 [ class "title" ]
            [ text "Layers" ]
        ]
    , nav [ class "nav-group" ]
        (List.map
            (\layer ->
                case layer of
                    Layer.Image imageData ->
                        a [ class "nav-group-item active" ]
                            [ span [ class "icon icon-picture" ] []
                            , text imageData.name
                            ]

                    Layer.Object objectData ->
                        a [ class "nav-group-item" ]
                            [ span [ class "icon icon-network" ] []
                            , text objectData.name
                            ]

                    Layer.Tile tileData ->
                        a [ class "nav-group-item" ]
                            [ span [ class "icon icon-layout" ] []
                            , text tileData.name
                            ]

                    Layer.InfiniteTile tileChunkedData ->
                        a [ class "nav-group-item" ]
                            [ span [ class "icon icon-layout" ] []
                            , text tileChunkedData.name
                            ]
            )
            l
        )
    ]
