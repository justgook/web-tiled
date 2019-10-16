module WebTiled.PanelTiled exposing (Model(..), view)

import Dict
import Html exposing (Attribute, Html, a, button, div, h1, header, input, nav, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes as Html exposing (class, colspan, style, type_)
import Html.Lazy
import Tiled.Layer as Layer
import Tiled.Level as Tiled
import Tiled.Properties exposing (Properties, Property(..))
import Tiled.Tileset as Tileset
import Tiled.Util
import WebTiled.Internal.List as List
import WebTiled.PanelTiled.Render as RenderPanel
import WebTiled.PanelTiled.Tileset as TilesetPanel
import WebTiled.UI.Tree exposing (Height, Width)


type Model
    = MainTools
    | LayerTools
    | ObjectTools
    | Properties
    | LevelProperties
    | Layers
    | Tilesets
    | Render


view : String -> Tiled.Level -> Width -> Height -> Model -> Html msg
view relUrl level w_ h_ m =
    let
        tilesetActiveTab =
            0
    in
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
            tilesets tilesetActiveTab relUrl (Tiled.Util.levelData level).tilesets |> panel w_ h_ "Tilesets"

        Render ->
            panel w_ h_ "Render" [ RenderPanel.view level ]


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


tilesets : Int -> String -> List Tileset.Tileset -> List (Html msg)
tilesets activeTab relUrl t =
    [ Html.Lazy.lazy2 tilesetsTabsWrap activeTab t
    , div
        [ style "background-color" "#BFBFBF"
        , style "flex" "1"
        , style "position" "relative"
        ]
        [ Html.Lazy.lazy3 tilesetsContentWrap relUrl t activeTab ]
    ]


tilesetsTabsWrap : Int -> List Tileset.Tileset -> Html msg
tilesetsTabsWrap activeTab t =
    let
        pluss =
            div [ class "tab-item tab-item-fixed" ]
                [ span [ class "icon icon-plus" ]
                    []
                ]
    in
    List.indexedFoldl
        (\i tileset acc ->
            let
                tabAttrs =
                    if i == activeTab then
                        class "tab-item active"

                    else
                        class "tab-item"
            in
            case tileset of
                Tileset.Source sourceTileData ->
                    div [ tabAttrs ]
                        [ span [ class "icon icon-cancel icon-close-tab" ] []
                        , text "Source"
                        ]
                        :: acc

                Tileset.Embedded embeddedTileData ->
                    div
                        [ tabAttrs
                        , style "text-overflow" "ellipsis"
                        , style "white-space" "nowrap"
                        , style "overflow" "hidden"
                        ]
                        [ span [ class "icon icon-cancel icon-close-tab" ] []
                        , text embeddedTileData.name
                        ]
                        :: acc

                Tileset.ImageCollection imageCollectionTileData ->
                    div [ tabAttrs ]
                        [ span [ class "icon icon-cancel icon-close-tab" ] []
                        , text imageCollectionTileData.name
                        ]
                        :: acc
        )
        []
        t
        |> (\a -> a ++ [ pluss ])
        |> div
            [ class "tab-group"
            , style "overflow-x" "scroll"
            , style "max-width" "100%"
            ]


tilesetsContentWrap : String -> List Tileset.Tileset -> Int -> Html msg
tilesetsContentWrap relUrl_ t_ activeTab_ =
    div
        [ style "position" "absolute"
        , style "top" "0"
        , style "right" "0"
        , style "bottom" "0"
        , style "left" "0"
        , style "overflow" "scroll"
        ]
        (TilesetPanel.view relUrl_ t_ activeTab_)


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
            input [ type_ "color", Html.value color ] []

        PropInt value ->
            text <| String.fromInt value

        PropFloat value ->
            text <| fromFloat value

        PropBool value ->
            input [ type_ "checkbox", Html.checked value ] []

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
