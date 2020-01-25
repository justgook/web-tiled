module WebTiled.Panel.Tilesets exposing (imagesFromDisk, imagesFromUrl, view)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, classList, style)
import Html.Events exposing (onClick)
import Html.Lazy as Lazy
import IDE.Internal.List as List
import IDE.UI.Transform.DragScale as DragScale
import Tiled.Tileset as Tileset
import TypedSvg as Svg
import TypedSvg.Attributes
import TypedSvg.Attributes.InPx as SvgInPx
import WebTiled.Message exposing (Message(..))
import WebTiled.Panel.Generic as Generic
import WebTiled.Svg.Tileset


view : Int -> Int -> Int -> List Tileset.Tileset -> Dict Int Tileset.Tileset -> Html Message
view w h selected tilesets external =
    [ Lazy.lazy3 viewTabs selected tilesets external
    , Lazy.lazy3 viewContent selected tilesets external
    ]
        |> div []
        |> Generic.panel w h "Tilesets"


viewContent : Int -> List Tileset.Tileset -> Dict Int Tileset.Tileset -> Html msg
viewContent selected tilesets external =
    div
        [ style "overflow" "hidden"
        , style "position" "absolute"
        , style "top" "32px"
        , style "right" "0"
        , style "bottom" "0"
        , style "left" "0"
        ]
    <|
        List.indexedFoldl (content selected external) [] tilesets


viewTabs : Int -> List Tileset.Tileset -> Dict Int Tileset.Tileset -> Html Message
viewTabs active tilesets external =
    header [ class "toolbar toolbar-header text-center" ]
        [ div [ class "toolbar-actions" ]
            [ List.indexedFoldl (tabs active external) [] tilesets
                |> List.reverse
                |> div [ class "btn-group" ]
            ]
        ]


imagesFromUrl : String -> List Tileset.Tileset -> Dict Int Tileset.Tileset -> Html msg
imagesFromUrl url tilesets external =
    List.indexedFoldl
        (\i tileset acc ->
            case tileset of
                Tileset.Source { firstgid } ->
                    case Dict.get firstgid external of
                        Just (Tileset.Embedded info) ->
                            Lazy.lazy4 image (toFloat info.imagewidth) (toFloat info.imageheight) (imageId i) (url ++ info.image) :: acc

                        Just (Tileset.ImageCollection info) ->
                            acc

                        _ ->
                            acc

                Tileset.Embedded info ->
                    Lazy.lazy4 image (toFloat info.imagewidth) (toFloat info.imageheight) (imageId i) (url ++ info.image) :: acc

                Tileset.ImageCollection { name } ->
                    acc
        )
        []
        (tilesets ++ Dict.values external)
        |> div []


imagesFromDisk : Dict String String -> List Tileset.Tileset -> Html msg
imagesFromDisk images tilesets =
    List.indexedFoldl
        (\i tileset acc ->
            case tileset of
                Tileset.Source sourceTileData ->
                    acc

                Tileset.Embedded info ->
                    Lazy.lazy4 image
                        (toFloat info.imagewidth)
                        (toFloat info.imageheight)
                        (imageId i)
                        (Dict.get info.image images |> Maybe.withDefault "")
                        :: acc

                Tileset.ImageCollection { name } ->
                    acc
        )
        []
        tilesets
        |> div []


content : Int -> Dict Int Tileset.Tileset -> Int -> Tileset.Tileset -> List (Html msg) -> List (Html msg)
content active external i tileset acc =
    case tileset of
        Tileset.Source { firstgid } ->
            case Dict.get firstgid external of
                Just ((Tileset.Embedded _) as tileset_) ->
                    content active external i tileset_ acc

                Just ((Tileset.ImageCollection _) as tileset_) ->
                    content active external i tileset_ acc

                _ ->
                    acc

        Tileset.Embedded info ->
            let
                m =
                    { drag = { x = 0, y = 0 }, scale = 1 }

                lattr =
                    if active /= i then
                        [ DragScale.apply m, style "display" "none" ]

                    else
                        [ DragScale.apply m ]

                internalSpacing =
                    1
            in
            div lattr
                [ Lazy.lazy7 tiles internalSpacing (imageId i) info.firstgid info.tilecount info.columns info.tilewidth info.tileheight
                ]
                :: acc

        Tileset.ImageCollection { name } ->
            acc


imageId : Int -> String
imageId i =
    "tileset-image[" ++ String.fromInt i ++ "]"


tiles : Float -> String -> Int -> Int -> Int -> Int -> Int -> Html msg
tiles internalSpacing imageId_ firstgid tilecount columns tilewidth tileheight =
    let
        rows =
            tilecount // columns

        w =
            toFloat (columns * tilewidth)
                |> (+) (toFloat columns * internalSpacing)

        h =
            toFloat (rows * tileheight)
                |> (+) (toFloat rows * internalSpacing)
    in
    Svg.svg
        [ SvgInPx.width w, SvgInPx.height h, TypedSvg.Attributes.viewBox 0 0 w h ]
        (WebTiled.Svg.Tileset.tiles internalSpacing
            imageId_
            { firstgid = firstgid
            , tilecount = tilecount
            , columns = columns
            , tilewidth = tilewidth
            , tileheight = tileheight
            }
        )


image : Float -> Float -> String -> String -> Html msg
image w h imageId_ url =
    Svg.svg [ TypedSvg.Attributes.style "display:none" ]
        [ Svg.defs []
            [ WebTiled.Svg.Tileset.image w h imageId_ url
            ]
        ]


tabs active external i tileset =
    case tileset of
        Tileset.Source { source, firstgid } ->
            case Dict.get firstgid external of
                Just ((Tileset.Embedded _) as tileset_) ->
                    tabs active external i tileset_

                Just ((Tileset.ImageCollection _) as tileset_) ->
                    tabs active external i tileset_

                _ ->
                    (::) <| tilesetButton active i ("Loading (" ++ source ++ ")")

        Tileset.Embedded { name } ->
            (::) <| tilesetButton active i name

        Tileset.ImageCollection { name } ->
            (::) <| tilesetButton active i name


tilesetButton active i name =
    button
        [ onClick (SelectTileset i)
        , classList [ ( "active", active == i ) ]
        , class "btn btn-mini btn-default"
        ]
        [ text name ]


toMaybe : Tileset.Tileset -> Maybe Tileset.Tileset
toMaybe tileset =
    case tileset of
        Tileset.Source _ ->
            Nothing

        _ ->
            Just tileset
