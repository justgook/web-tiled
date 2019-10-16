module WebTiled.UI.Html exposing (Message(..), Model, ResizeInfo, update, view)

import Html exposing (Attribute, Html, button, div, text)
import Html.Attributes as Html exposing (class, style)
import Html.Events
import Json.Decode as Decode
import WebTiled.UI.Render exposing (Config)
import WebTiled.UI.Tree exposing (Height, Node, Path, Width)


type alias Model panel =
    { node : WebTiled.UI.Tree.Node panel
    , resizeInfo : Maybe ResizeInfo
    }


type Message
    = ResizeStart ResizeInfo
    | ResizeStop ResizeInfo
    | Resizing ResizeInfo
    | SetTab Path


type alias ResizeInfo =
    { path : ( Path, Path )
    , start :
        { x : Int
        , y : Int
        }
    , diffX : Int
    , diffY : Int
    , axis : Axis
    }


type Axis
    = Vertical
    | Horizontal


update : Message -> Model panel -> Model panel
update msg model =
    case msg of
        ResizeStart info ->
            { model | resizeInfo = Just info }

        ResizeStop _ ->
            { model | resizeInfo = Nothing }

        Resizing ({ path, diffX, diffY, axis } as info) ->
            let
                ( p1, p2 ) =
                    path
            in
            case axis of
                Horizontal ->
                    { model
                        | node =
                            WebTiled.UI.Tree.mapSizeAt p1 ((+) diffX) identity model.node
                                |> WebTiled.UI.Tree.mapSizeAt p2 (\a -> a - diffX) identity
                        , resizeInfo = Just info
                    }

                Vertical ->
                    { model
                        | node =
                            WebTiled.UI.Tree.mapSizeAt p1 identity ((+) diffY) model.node
                                |> WebTiled.UI.Tree.mapSizeAt p2 identity (\a -> a - diffY)
                        , resizeInfo = Just info
                    }

        --            { model
        --                | node =
        --                    WebTiled.UI.Tree.mapSizeAt p1 ((+) diffX) ((+) diffY) model.node
        --                        |> WebTiled.UI.Tree.mapSizeAt p2 (\a -> a - diffX) (\a -> a - diffY)
        --                , resizeInfo = Just info
        --            }
        SetTab _ ->
            Debug.todo "SetTab"


view : (Width -> Height -> panel -> Html Message) -> Model panel -> Html Message
view drawPanel { node, resizeInfo } =
    let
        newConfig =
            config drawPanel

        attrs =
            dragMove resizeInfo [ style "width" "100%", style "height" "100%" ]
    in
    [ WebTiled.UI.Render.view [] newConfig node ]
        |> div attrs


config :
    (Int -> Int -> panel -> Html Message)
    -> Config panel String (Html Message)
config panel =
    let
        tabs =
            List.map
                (\( path, title ) ->
                    button
                        [ Html.style "background" "rgb(51,51,51)"
                        , Html.style "padding" "4px"
                        , Html.style "margin" "2px"
                        ]
                        [ text title
                        ]
                )

        setWidth : Width -> Attribute msg
        setWidth w =
            String.fromInt w ++ "px" |> style "width"

        setHeight : Height -> Attribute msg
        setHeight w =
            String.fromInt w ++ "px" |> style "height"

        vertical : Width -> List (Html Message) -> Html Message
        vertical w =
            div [ setWidth w ]

        horizontal : Height -> List (Html Message) -> Html Message
        horizontal h =
            div
                [ setHeight h
                , style "align-items" "stretch"
                , style "display" "flex"

                --                , style "overflow" "hidden"
                ]

        tabsNorth l =
            div [] [ text "tabsNorth" ]

        tabsEast l =
            div [] [ text "tabsEast" ]

        tabsSouth l =
            div [] [ text "tabsSouth" ]

        tabsWest l =
            div [] [ text "tabsWest" ]
    in
    { vResize = vResize
    , hResize = hResize
    , vertical = vertical
    , horizontal = horizontal
    , tabsNorth = tabsNorth
    , tabsEast = tabsEast
    , tabsSouth = tabsSouth
    , tabsWest = tabsWest
    , panel = panel
    }


vResize : Path -> Path -> Html Message
vResize path1 path2 =
    div
        [ style "background" "#ddd"
        , style "cursor" "row-resize"
        , dragStart Vertical path1 path2
        , class "vResize"
        ]
        []


hResize : Path -> Path -> Html Message
hResize path1 path2 =
    div
        [ style "background" "#ddd"
        , style "cursor" "col-resize"
        , dragStart Horizontal path1 path2
        , class "hResize"
        ]
        []


dragStart : Axis -> Path -> Path -> Attribute Message
dragStart axis path1 path2 =
    let
        decoder =
            Decode.map2
                (\x y ->
                    { message =
                        ResizeStart
                            { path = ( path1, path2 )
                            , diffX = 0
                            , diffY = 0
                            , axis = axis
                            , start =
                                { x = x
                                , y = y
                                }
                            }
                    , stopPropagation = True
                    , preventDefault = True
                    }
                )
                (Decode.field "clientX" Decode.int)
                (Decode.field "clientY" Decode.int)
    in
    Html.Events.custom "mousedown" decoder


dragMove : Maybe ResizeInfo -> List (Attribute Message) -> List (Attribute Message)
dragMove resizing =
    case resizing of
        Just info ->
            let
                decoder =
                    Decode.map2
                        (\x y ->
                            { message =
                                Resizing
                                    { info
                                        | diffX = x
                                        , diffY = y
                                    }
                            , stopPropagation = True
                            , preventDefault = True
                            }
                        )
                        (Decode.field "movementX" Decode.int)
                        (Decode.field "movementY" Decode.int)
            in
            decoder
                |> Html.Events.custom "mousemove"
                |> (\a attrs ->
                        a
                            :: dragStop info
                            :: attrs
                   )

        _ ->
            identity


dragStop : ResizeInfo -> Html.Attribute Message
dragStop info =
    { message = ResizeStop info
    , stopPropagation = True
    , preventDefault = True
    }
        |> Decode.succeed
        |> Html.Events.custom "mouseup"
