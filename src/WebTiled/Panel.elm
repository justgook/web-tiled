module WebTiled.Panel exposing (Message(..), Model, block, init, view)

import Dict exposing (Dict)
import Html exposing (Attribute, Html, a, button, div, h1, header, nav, span, text)
import Html.Attributes exposing (class, style)
import IDE.UI.Tree as UI
import Tiled.Layer as Layer
import Tiled.Level as Tiled
import Tiled.Util
import WebTiled.Kind exposing (Kind(..))
import WebTiled.Panel.FileManager as FileManager
import WebTiled.Panel.LevelProperties as LevelPropertiesPanel
import WebTiled.Panel.Preferences as Preferences
import WebTiled.Panel.Properties exposing (propertiesTable)
import WebTiled.Panel.Render as RenderPanel
import WebTiled.Panel.Tileset as TilesetPanel
import WebTiled.Panel.TopMenu as TopMenu


block =
    let
        default =
            { xMin = 10
            , xMax = Nothing
            , yMin = 10
            , yMax = Nothing
            }
    in
    { mainTools =
        UI.node MainTools
            |> UI.setLimits
                { yMax = Just 34
                , yMin = 34
                , xMax = Just 222
                , xMin = 220
                }
    , layerTools =
        UI.node LayerTools
            |> UI.setLimits
                { yMax = Just 34
                , yMin = 34
                , xMax = Nothing
                , xMin = 206
                }
    , objectTools = UI.node ObjectTools |> UI.setLimits { default | xMax = Just 250 }
    , properties = UI.node Properties |> UI.setLimits { default | xMax = Just 230 }
    , levelProperties = UI.node LevelProperties |> UI.setLimits { default | xMax = Just 250 }
    , layers = UI.node Layers |> UI.setLimits { default | xMax = Just 250 }
    , tilesets = UI.node Tilesets |> UI.setLimits { default | xMax = Just 250 }
    , render = UI.node Render |> UI.setLimits { default | xMin = 200 }
    , fileManager = UI.node FileManager |> UI.setLimits default
    , topMenu = UI.node TopMenu |> UI.setLimits { default | yMin = 22, yMax = Just 22 }
    , preferences = UI.node Preferences
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
    , topMenu : TopMenu.Model
    , preferences : Preferences.Model
    , widgetCache : { number : Dict String String }
    }


init : Model
init =
    { render = RenderPanel.init
    , tilesets = TilesetPanel.init
    , fileManager = FileManager.init
    , topMenu = TopMenu.init
    , preferences = Preferences.init
    , widgetCache = { number = Dict.empty }
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
            panel w_ h_ "Layers" (layers (Tiled.Util.getLevelData level).layers)
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
                |> bare w_ h_
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

        TopMenu ->
            TopMenu.view editor.topMenu

        Preferences ->
            Preferences.view editor.preferences


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
    [ nav [ class "nav-group sidebar" ]
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
