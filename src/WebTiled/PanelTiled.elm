module WebTiled.PanelTiled exposing (Kind(..), Model, init, view)

import Dict
import Html exposing (Attribute, Html, a, button, div, h1, header, input, nav, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, colspan, style, type_)
import Html.Lazy
import IDE.Internal.List as List
import IDE.UI.Tree exposing (Height, Width)
import Tiled.Layer as Layer
import Tiled.Level as Tiled
import Tiled.Properties exposing (Properties, Property(..))
import Tiled.Tileset as Tileset
import Tiled.Util
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


type alias Model =
    { render : RenderPanel.Model
    , tilesets : TilesetPanel.Model
    }


init : Model
init =
    { render = RenderPanel.init
    , tilesets = TilesetPanel.init
    }


view : Model -> String -> Tiled.Level -> Width -> Height -> Kind -> Html (Model -> Model)
view editor relUrl level w_ h_ m =
    case m of
        MainTools ->
            bare w_ h_ [ mainToolbar ]

        LayerTools ->
            bare w_ h_ [ layerToolbar ]

        ObjectTools ->
            bare w_ h_ []

        LevelProperties ->
            panel w_ h_ "Properties" (propertiesTable ( "Map", levelProperties level ) ((Tiled.Util.levelData level).properties |> customProps))

        Properties ->
            panel w_ h_ "Properties" (propertiesTable ( "Object", [] ) [])

        Layers ->
            bare w_ h_ (layers (Tiled.Util.levelData level).layers)

        Tilesets ->
            [ TilesetPanel.view editor.tilesets relUrl (Tiled.Util.levelData level).tilesets
                |> Html.map (\fn model -> { model | tilesets = fn model.tilesets })
            ]
                |> panel w_ h_ "Tilesets"

        Render ->
            panel w_
                h_
                "Render"
                [ RenderPanel.view editor.render level
                    |> Html.map (\fn model -> { model | render = fn model.render })
                ]


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


bare : Width -> Height -> List (Html msg) -> Html msg
bare w_ h_ =
    let
        setWidth : Width -> Attribute msg
        setWidth w =
            String.fromInt w ++ "px" |> style "width"

        setHeight : Height -> Attribute msg
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


levelProperties : Tiled.Level -> List (Html msg)
levelProperties l =
    let
        info =
            Tiled.Util.levelData l
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


propertiesTable : ( String, List (Html msg) ) -> List (Html msg) -> List (Html msg)
propertiesTable ( mainText, props1 ) props2 =
    [ table [ class "table-striped" ]
        [ thead []
            [ tr []
                [ th []
                    [ text "Property" ]
                , th []
                    [ text "Value" ]
                ]
            ]
        , tbody []
            [ tr []
                [ th [ colspan 2, style "background-color" "#dcdfe1" ]
                    [ text mainText ]
                ]
            ]
        , tbody [] props1
        , tbody []
            [ tr []
                [ th [ colspan 2, style "background-color" "#dcdfe1" ]
                    [ text "Custom Properties" ]
                ]
            ]
        , tbody [] props2
        ]
    ]


customProps : Properties -> List (Html msg)
customProps =
    Dict.foldl (\k v acc -> propertyRow k v :: acc) []


propertyRow key prop =
    tr []
        [ td []
            [ text key ]
        , td []
            [ propToWidget prop ]
        ]


propToWidget prop =
    case prop of
        PropString value ->
            text value

        PropColor value ->
            let
                color =
                    if value == "" then
                        "#111111"

                    else
                        value
            in
            input [ type_ "color", Html.Attributes.value color ] []

        PropInt value ->
            text <| String.fromInt value

        PropFloat value ->
            text <| fromFloat value

        PropBool value ->
            input [ type_ "checkbox", Html.Attributes.checked value ] []

        _ ->
            text "propertyRow"


fromFloat : Float -> String
fromFloat f =
    let
        s =
            String.fromFloat f
    in
    if String.contains "." s || String.contains "e" s then
        s

    else
        s ++ ".0"
