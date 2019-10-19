module WebTiled.Svg.Tileset exposing (header, view)

import Html exposing (div)
import Html.Lazy
import IDE.Internal.List as List
import Tiled.Tileset as Tileset
import TypedSvg exposing (..)
import TypedSvg.Attributes exposing (viewBox, xlinkHref)
import TypedSvg.Attributes.InPx exposing (..)
import TypedSvg.Core exposing (Svg, attribute)


header : String -> List Tileset.Tileset -> Svg a
header relUrl t =
    List.indexedFoldl
        (\i tileset acc ->
            case tileset of
                Tileset.Source sourceTileData ->
                    acc

                Tileset.Embedded info ->
                    let
                        imageId =
                            "tileset-image[" ++ String.fromInt i ++ "]"
                    in
                    tilesHeader
                        (defs []
                            [ image
                                [ xlinkHref <| relUrl ++ info.image
                                , width <| toFloat info.imagewidth
                                , height <| toFloat info.imageheight
                                , id imageId
                                ]
                                []
                            ]
                            :: acc
                        )
                        imageId
                        info

                Tileset.ImageCollection imageCollectionTileData ->
                    acc
        )
        []
        t
        |> svg [ width 0, height 0, viewBox 0 0 0 0 ]


view : String -> List Tileset.Tileset -> Int -> List (Svg msg)
view relUrl t activeTab =
    List.indexedFoldl
        (\i tileset acc ->
            case tileset of
                Tileset.Source sourceTileData ->
                    acc

                Tileset.Embedded info ->
                    let
                        lattr =
                            if activeTab /= i then
                                [ TypedSvg.Attributes.style "display:none" ]

                            else
                                []
                    in
                    div lattr [ Html.Lazy.lazy3 forLazy relUrl i info ]
                        :: acc

                Tileset.ImageCollection imageCollectionTileData ->
                    acc
        )
        []
        t


forLazy relUrl i info =
    let
        spacing =
            3

        imageId =
            "tileset-image[" ++ String.fromInt i ++ "]"

        rows =
            info.tilecount // info.columns

        w =
            toFloat (info.columns * info.tilewidth)
                |> (+) (toFloat info.columns * spacing)

        h =
            toFloat (rows * info.tileheight)
                |> (+) (toFloat rows * spacing)
    in
    svg
        [ width w, height h, viewBox 0 0 w h ]
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
            , tileSymbol tileGid x_ y_ w h imageId
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


tileSymbol tileGid x_ y_ w h imageId =
    g [ TypedSvg.Attributes.style "display:none" ]
        [ svg
            [ id tileGid
            , viewBox x_ y_ w h
            , width w
            , height h
            ]
            [ use
                [ xlinkHref <| "#" ++ imageId
                , width w
                , height h
                ]
                []
            ]
        ]


tilesHeader : List (Svg msg) -> String -> { a | firstgid : Int, tilewidth : Int, tileheight : Int, columns : Int, tilecount : Int } -> List (Svg msg)
tilesHeader acc =
    tilesHeader_ ( 0, acc )


tilesHeader_ : ( Int, List (Svg msg) ) -> String -> { a | firstgid : Int, tilewidth : Int, tileheight : Int, columns : Int, tilecount : Int } -> List (Svg msg)
tilesHeader_ ( i, acc ) imageId info =
    let
        data =
            getData info.firstgid info.columns info.tilewidth info.tileheight i
    in
    if i < info.tilecount then
        tilesHeader_
            ( i + 1
            , tileSymbol data.tileGid data.x data.y data.w data.h imageId
                :: acc
            )
            imageId
            info

    else
        acc


getData firstgid columns tilewidth tileheight i =
    let
        column =
            toFloat (remainderBy columns i)

        row =
            toFloat (i // columns)

        w =
            toFloat tilewidth

        h =
            toFloat tileheight
    in
    { tileGid = String.fromInt (i + firstgid)
    , w = w
    , h = h
    , x = w * column
    , y = h * row
    }


id =
    attribute "id"
