module WebTiled.Panel.Tileset exposing (Model, init, view)

import Dict exposing (Dict)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class, style)
import Html.Events
import Html.Lazy
import IDE.Internal.List as List
import IDE.UI.Transform.DragScale as DragScale exposing (DragScale)
import Json.Decode as D
import Tiled.Tileset as Tileset
import TypedSvg.Events
import VirtualDom exposing (Handler(..))
import WebTiled.DropFiles exposing (File)
import WebTiled.Svg.Tileset


type alias Model =
    DragScale
        { activeTab : Int
        , gid : List Int
        }


init : Model
init =
    { scale = 1
    , drag =
        { x = 0
        , y = 0
        }
    , activeTab = 1
    , gid = []
    }


handlerWheel =
    MayPreventDefault (D.map (\fn -> ( \m -> fn m |> limit, True )) DragScale.wheel)


view : Model -> String -> Dict String File -> List Tileset.Tileset -> Html (Model -> Model)
view m relUrl files t =
    div
        [ style "height" "100%"
        ]
        [ Html.Lazy.lazy2 tilesetsTabsWrap m.activeTab t
        , div
            [ style "overflow" "hidden"
            , style "height" "100%"
            , TypedSvg.Events.on "wheel" handlerWheel
            ]
            [ div
                [ DragScale.apply m
                , style "transform-origin" "0 0"
                ]
                [ Html.Lazy.lazy4 tilesetsContentWrap relUrl files t m.activeTab ]
            ]
        ]


tilesetsContentWrap relUrl_ files t_ active =
    div [] (WebTiled.Svg.Tileset.view relUrl_ files t_ active)


tilesetsTabsWrap : Int -> List Tileset.Tileset -> Html (Model -> Model)
tilesetsTabsWrap activeTab t =
    let
        pluss =
            div [ class "tab-item tab-item-fixed" ]
                [ span [ class "icon icon-plus" ] []
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
                        , Html.Events.onClick (activate i)
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

            --, style "overflow-x" "scroll"
            , style "max-width" "100%"
            ]


limit m =
    { m
        | drag = { x = min m.drag.x 0, y = min m.drag.y 0 }
    }


getActiveSize i b =
    { w = 100, h = 100 }


activate i m =
    { init | activeTab = i }


{-| Returns `Just` the element at the given index in the list,
or `Nothing` if the index is out of range.
-}
getAt : Int -> List a -> Maybe a
getAt idx xs =
    if idx < 0 then
        Nothing

    else
        List.head <| List.drop idx xs
