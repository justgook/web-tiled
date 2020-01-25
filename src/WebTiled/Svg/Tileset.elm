module WebTiled.Svg.Tileset exposing (image, tiles)

import TypedSvg exposing (..)
import TypedSvg.Attributes exposing (viewBox, xlinkHref)
import TypedSvg.Attributes.InPx exposing (..)
import TypedSvg.Core exposing (Svg, attribute)


image : Float -> Float -> String -> String -> Svg msg
image w h imageId url =
    TypedSvg.image
        [ xlinkHref <| url
        , id imageId
        , width w
        , height h
        ]
        []


tiles : Float -> String -> { a | firstgid : Int, tilewidth : Int, tileheight : Int, columns : Int, tilecount : Int } -> List (Svg msg)
tiles spacing imageId =
    tiles_ ( 0, [] ) spacing imageId


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
