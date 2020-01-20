module WebTiled.Panel2 exposing (Model, init, preferences, render, update)

import Dict exposing (Dict)
import Html exposing (Attribute, Html, a, button, div, h1, header, nav, span, text)
import Html.Attributes exposing (class, style)
import IDE.UI.Tree as UI
import Tiled.Layer as Layer
import Tiled.Level as Tiled
import Tiled.Util
import WebTiled.Kind exposing (Kind(..))
import WebTiled.Message exposing (Message(..))
import WebTiled.Panel.FileManager as FileManager
import WebTiled.Panel.LevelProperties as LevelPropertiesPanel
import WebTiled.Panel.Preferences as Preferences
import WebTiled.Panel.Properties exposing (propertiesTable)
import WebTiled.Panel.Render as RenderPanel
import WebTiled.Panel.Tileset as TilesetPanel
import WebTiled.Panel.TopMenu as TopMenu


preferences : UI.Tree Kind
preferences =
    UI.node Preferences


topMenu : UI.Tree Kind
topMenu =
    UI.node TopMenu


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


update msg model =
    case msg of
        ShowPreferences category ->
            ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


render model level w_ h_ kind =
    case kind of
        TopMenu ->
            TopMenu.view model.topMenu

        Preferences ->
            Preferences.view model.preferences

        _ ->
            span [] []



--
--
--panel w_ h_ title content =
--    bare w_
--        h_
--        [ header [ class "toolbar toolbar-header" ]
--            [ h1 [ class "title" ]
--                [ text title ]
--            ]
--        , div
--            [ class "pane"
--            , style "background" "#BFBFBF"
--            , style "display" "flex"
--            , style "flex-flow" "column"
--            ]
--            content
--        ]
--
--
--bare w_ h_ =
--    let
--        setWidth w =
--            String.fromInt w ++ "px" |> style "width"
--
--        setHeight w =
--            String.fromInt w ++ "px" |> style "height"
--    in
--    div
--        [ setWidth w_
--        , setHeight h_
--        , style "display" "flex"
--        , style "flex-flow" "column"
--        , style "position" "relative"
--        ]
--
--
--mainToolbar =
--    header [ class "toolbar toolbar-header" ]
--        [ div [ class "toolbar-actions" ]
--            [ div [ class "btn-group" ]
--                [ button [ class "btn btn-default" ]
--                    [ span [ class "icon icon-doc-text" ]
--                        []
--                    ]
--                , button [ class "btn btn-default" ]
--                    [ span [ class "icon icon-folder" ] []
--                    ]
--                , button [ class "btn btn-default" ] [ span [ class "icon icon-floppy" ] [] ]
--                ]
--            , div
--                [ class "btn-group" ]
--                [ button [ class "btn btn-default active" ]
--                    [ span [ class "icon icon-reply" ]
--                        []
--                    ]
--                , button [ class "btn btn-default" ]
--                    [ span [ class "icon icon-forward" ]
--                        []
--                    ]
--                ]
--            , button [ class "btn btn-default" ]
--                [ span [ class "icon icon-tools" ]
--                    []
--                ]
--            ]
--        ]
--
--
--layerToolbar =
--    header [ class "toolbar toolbar-header" ]
--        [ div [ class "toolbar-actions" ]
--            [ div [ class "btn-group" ]
--                [ button [ class "btn btn-default" ] [ span [ class "icon flaticon-rubber-stamp" ] [] ]
--                , button [ class "btn btn-default" ] [ span [ class "icon flaticon-paint-bucket-2" ] [] ]
--                , button [ class "btn btn-default" ] [ span [ class "icon flaticon-cropping-tool-point-1" ] [] ]
--                , button [ class "btn btn-default" ] [ span [ class "icon flaticon-eraser" ] [] ]
--                , button [ class "btn btn-default" ] [ span [ class "icon flaticon-dotted-square" ] [] ]
--                , button [ class "btn btn-default" ] [ span [ class "icon flaticon-magic-wand-with-a-star" ] [] ]
--                ]
--            ]
--        ]
--
--
--layers l =
--    [ nav [ class "nav-group sidebar" ]
--        (List.map
--            (\layer ->
--                case layer of
--                    Layer.Image imageData ->
--                        a [ class "nav-group-item active" ]
--                            [ span [ class "icon icon-picture" ] []
--                            , text imageData.name
--                            ]
--
--                    Layer.Object objectData ->
--                        a [ class "nav-group-item" ]
--                            [ span [ class "icon icon-network" ] []
--                            , text objectData.name
--                            ]
--
--                    Layer.Tile tileData ->
--                        a [ class "nav-group-item" ]
--                            [ span [ class "icon icon-layout" ] []
--                            , text tileData.name
--                            ]
--
--                    Layer.InfiniteTile tileChunkedData ->
--                        a [ class "nav-group-item" ]
--                            [ span [ class "icon icon-layout" ] []
--                            , text tileChunkedData.name
--                            ]
--            )
--            l
--        )
--    ]
