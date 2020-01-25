module WebTiled.Panel.Layers exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Tiled.Layer as Layer
import WebTiled.Message exposing (Message(..))
import WebTiled.Panel.Generic as Generic


view : Int -> Int -> List Int -> List Layer.Layer -> Html Message
view w h selected =
    List.indexedMap
        (\i layer ->
            case layer of
                Layer.Image imageData ->
                    item "icon-picture" imageData.name selected i

                Layer.Object objectData ->
                    item "icon-network" objectData.name selected i

                Layer.Tile tileData ->
                    item "icon-layout" tileData.name selected i

                Layer.InfiniteTile tileChunkedData ->
                    item "icon-layout" tileChunkedData.name selected i
        )
        >> nav [ class "nav-group sidebar" ]
        >> List.singleton
        >> div []
        >> Generic.panel w h "Layers"


item icon name selected i =
    a
        [ class "nav-group-item"

        --, draggable "true"
        , onClick (SelectLayer i)
        , classList [ ( "active", List.member i selected ) ]
        ]
        [ span [ class "icon", class icon ] []
        , text name
        ]
