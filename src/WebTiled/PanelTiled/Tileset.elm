module WebTiled.PanelTiled.Tileset exposing (view)

import Html exposing (Html)
import Tiled.Tileset as Tileset
import TypedSvg exposing (..)
import TypedSvg.Attributes exposing (viewBox, xlinkHref)
import TypedSvg.Attributes.InPx exposing (..)
import TypedSvg.Core exposing (Svg, attribute)
import WebTiled.Internal.List as List


view : String -> List Tileset.Tileset -> Int -> List (Html msg)
view relUrl t activeTab =
    let
        spacing =
            3
    in
    List.indexedFoldl
        (\i tileset acc ->
            case tileset of
                Tileset.Source sourceTileData ->
                    acc

                Tileset.Embedded info ->
                    let
                        rows =
                            info.tilecount // info.columns

                        w =
                            toFloat (info.columns * info.tilewidth)
                                |> (+) (toFloat info.columns * spacing)

                        h =
                            toFloat (rows * info.tileheight)
                                |> (+) (toFloat rows * spacing)

                        imageId =
                            "tileset-image[" ++ String.fromInt i ++ "]"

                        lattr =
                            if activeTab /= i then
                                [ TypedSvg.Attributes.style "display:block;width:0;height:0;" ]

                            else
                                []
                    in
                    svg
                        ([ width w
                         , height h
                         , viewBox 0 0 w h
                         ]
                            ++ lattr
                        )
                        (defs []
                            [ image
                                [ xlinkHref <| relUrl ++ info.image
                                , width <| toFloat info.imagewidth
                                , height <| toFloat info.imageheight
                                , id imageId
                                ]
                                []
                            ]
                            :: tiles spacing imageId info
                        )
                        :: acc

                Tileset.ImageCollection imageCollectionTileData ->
                    acc
        )
        []
        t


tiles =
    tiles_ ( 0, [] )


tiles_ ( i, acc ) spacing imageId info =
    let
        tileGid =
            String.fromInt (i + info.firstgid)

        w =
            toFloat info.tilewidth

        h =
            toFloat info.tileheight

        column =
            toFloat (remainderBy info.columns i)

        row =
            toFloat (i // info.columns)

        x_ =
            w * column

        y_ =
            h * row
    in
    if i < info.tilecount then
        tiles_
            ( i + 1
            , symbol
                [ id tileGid
                , viewBox x_ y_ w h
                , width w
                , height h
                ]
                [ use [ xlinkHref <| "#" ++ imageId ] []
                ]
                :: use
                    [ xlinkHref <| "#" ++ tileGid
                    , x (x_ + spacing * column)
                    , y (y_ + spacing * row)
                    ]
                    []
                :: acc
            )
            spacing
            imageId
            info

    else
        acc


id =
    attribute "id"
