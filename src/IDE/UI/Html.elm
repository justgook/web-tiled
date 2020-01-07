module IDE.UI.Html exposing (Message(..), Model, ResizeInfo, update, view)

import Html exposing (Attribute, Html, button, div, text)
import Html.Attributes exposing (class, style)
import Html.Events
import IDE.UI.Render exposing (Config)
import IDE.UI.Tree exposing (Height, Node, Path, Width)
import Json.Decode as Decode


type alias Model panel =
    { node : IDE.UI.Tree.Node panel
    , resizeInfo : Maybe ResizeInfo
    }


type Message msg
    = ResizeStart ResizeInfo
    | ResizeStop ResizeInfo
    | Resizing ResizeInfo
    | SetTab Path
    | Custom msg


type alias ResizeInfo =
    { path : ( Path, Path )
    , start : { x : Int, y : Int }
    , diffX : Int
    , diffY : Int
    , axis : Axis
    }


type Axis
    = Vertical
    | Horizontal


update : Message msg -> Model panel -> Model panel
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
                            IDE.UI.Tree.mapSizeAt p1 ((+) diffX) identity model.node
                                |> IDE.UI.Tree.mapSizeAt p2 (\a -> a - diffX) identity
                        , resizeInfo = Just info
                    }

                Vertical ->
                    { model
                        | node =
                            IDE.UI.Tree.mapSizeAt p1 identity ((+) diffY) model.node
                                |> IDE.UI.Tree.mapSizeAt p2 identity (\a -> a - diffY)
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

        Custom _ ->
            Debug.todo "Panel"


view : (Width -> Height -> panel -> Html msg) -> Model panel -> Html (Message msg)
view drawPanel { node, resizeInfo } =
    [ IDE.UI.Render.view [] (config drawPanel) node ]
        |> div (dragMove resizeInfo)


config panel =
    let
        tabs =
            List.map
                (\( path, title ) ->
                    button
                        [ style "background" "rgb(51,51,51)"
                        , style "padding" "4px"
                        , style "margin" "2px"
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

        --        vertical : Width -> List (Html Message) -> Html Message
        vertical w =
            div [ setWidth w ]

        --        horizontal : Height -> List (Html Message) -> Html Message
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
    , panel =
        \w h d ->
            panel w h d |> Html.map Custom
    }


vResize : Path -> Path -> Html (Message msg)
vResize path1 path2 =
    div
        [ style "background" "#ddd"
        , style "cursor" "row-resize"
        , dragStart Vertical path1 path2
        , class "vResize"
        ]
        []


hResize : Path -> Path -> Html (Message msg)
hResize path1 path2 =
    div
        [ style "background" "#ddd"
        , style "cursor" "col-resize"
        , dragStart Horizontal path1 path2
        , class "hResize"
        ]
        []


dragStart : Axis -> Path -> Path -> Attribute (Message msg)
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


dragMove : Maybe ResizeInfo -> List (Attribute (Message msg))
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
                |> (\a ->
                        a
                            :: dragStop info
                            :: []
                   )

        _ ->
            []


dragStop : ResizeInfo -> Attribute (Message msg)
dragStop info =
    { message = ResizeStop info
    , stopPropagation = True
    , preventDefault = True
    }
        |> Decode.succeed
        |> Html.Events.custom "mouseup"
