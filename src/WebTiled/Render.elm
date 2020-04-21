module WebTiled.Render exposing (Model, empty, initLevel, textureError, textureOption, update, view)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Playground exposing (..)
import Playground.Extra exposing (tile)
import Tiled
import Tiled.Layer as Layer exposing (Layer)
import Tiled.Level exposing (Level)
import Tiled.Object as Object
import Tiled.Tileset as Tileset
import WebGL exposing (Entity)
import WebGL.Shape2d
import WebGL.Texture as Texture exposing (Error(..), Texture, nonPowerOfTwoOptions)
import WebTiled.Util.Color as Color
import WebTiled.Util.Tiled as TiledUtil


empty : Model
empty =
    initMemory_


view : Model -> Html a
view =
    \m -> toHtml m


update : Model -> Model
update m =
    { m
        | entities =
            toEntities m
                |> WebGL.Shape2d.toEntities m.textures m.screen
                |> Tuple.first
    }


toHtml { entities, screen, clearColor } =
    entities
        |> WebGL.toHtmlWith
            [ WebGL.alpha True
            , WebGL.depth 1
            , clearColor
            ]
            [ Html.Attributes.width (floor screen.width)
            , Html.Attributes.height (floor screen.height)
            ]


type alias Model =
    Memory


type alias Memory =
    { tilewidth : Float
    , tileheight : Float
    , columns : Int
    , rows : Int
    , layers : List Layer
    , screen : Screen
    , textures : Dict String Texture
    , entities : List Entity
    , indexes : List { first : Int, last : Int, image : String }
    , clearColor : WebGL.Option
    }


initMemory_ : Memory
initMemory_ =
    { tilewidth = 1
    , tileheight = 1
    , columns = 1
    , rows = 1
    , layers = []
    , screen = toScreen 1 1
    , textures = Dict.empty
    , entities = []
    , indexes = []
    , clearColor = WebGL.clearColor 1 1 1 1
    }


getIndex indexes i =
    case indexes of
        item :: rest ->
            if item.first <= i && item.last >= i then
                ( i - item.first, item.image )

            else
                getIndex rest i

        [] ->
            ( 0, "" )


toEntities : Memory -> List Shape
toEntities { screen, tilewidth, tileheight, layers, columns, rows, indexes } =
    let
        tileset i =
            objTile tilewidth tileheight i

        objTile w h i =
            if i <= 0 then
                group []

            else
                let
                    { gid } =
                        Tiled.gidInfo i

                    ( rGid, image ) =
                        getIndex indexes gid
                in
                tile w h image rGid

        newX =
            screen.left + tilewidth * 0.5

        newY =
            screen.top - tileheight * 0.5
    in
    layers
        |> List.map
            (\layer ->
                case layer of
                    Layer.Tile { data } ->
                        List.indexedMap
                            (\i ->
                                tileset
                                    >> move
                                        (newX + toFloat (modBy columns i) * tilewidth)
                                        (newY - toFloat (i // columns) * tileheight)
                            )
                            data
                            |> group

                    Layer.Object { objects, opacity } ->
                        List.foldl
                            (\obj acc ->
                                case obj of
                                    Object.Tile item ->
                                        (objTile item.width item.height item.gid
                                            |> move (screen.left + item.x + item.width * 0.5) (screen.top - item.y + item.height * 0.5)
                                        )
                                            :: acc

                                    _ ->
                                        acc
                            )
                            []
                            objects
                            |> group
                            |> fade opacity

                    _ ->
                        [ rectangle red tilewidth tileheight ] |> group
            )


initLevel : Dict String Texture -> Level -> Model -> Model
initLevel textures level model =
    let
        info =
            TiledUtil.getLevelData level

        indexes =
            info.tilesets
                |> List.map indexTileset
                |> List.sortBy .first

        w =
            toFloat (info.width * info.tilewidth)

        h =
            toFloat (info.height * info.tileheight)

        { r, g, b } =
            info
                |> .backgroundcolor
                |> Color.fromHex
                |> Maybe.withDefault { r = 1, g = 1, b = 1 }
    in
    { model
        | tilewidth = toFloat info.tilewidth
        , tileheight = toFloat info.tileheight
        , columns = info.width
        , rows = info.height
        , layers =
            info.layers
        , clearColor = WebGL.clearColor r g b 1
        , textures = textures
        , screen = toScreen w h
        , indexes = indexes
    }
        |> update


indexTileset t =
    case t of
        Tileset.Source info ->
            { first = info.firstgid, last = 0, image = "" }

        Tileset.Embedded info ->
            { first = info.firstgid
            , last = info.firstgid + info.tilecount - 1
            , image = info.image
            }

        Tileset.ImageCollection info ->
            { first = info.firstgid, last = 0, image = "" }


textureError : Texture.Error -> String
textureError err =
    case err of
        LoadError ->
            "LoadError"

        SizeError _ _ ->
            "SizeError"


textureOption : Texture.Options
textureOption =
    { nonPowerOfTwoOptions
        | magnify = Texture.linear
        , minify = Texture.linear

        --, horizontalWrap = Texture.mirroredRepeat
        --, verticalWrap = Texture.mirroredRepeat
    }


toScreen : Float -> Float -> Screen
toScreen width height =
    { width = width
    , height = height
    , top = height / 2
    , left = -width / 2
    , right = width / 2
    , bottom = -height / 2
    }
