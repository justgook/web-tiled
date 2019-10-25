module IDE.UI.Transform.Select exposing (Select, handlerMouseDown, handlerMouseMove, mouseDown, select)

import Html
import Html.Attributes exposing (style)
import Json.Decode as D exposing (Decoder)
import VirtualDom exposing (Handler(..))


type alias Select a =
    { a
        | select :
            Maybe
                { startX : Float
                , startY : Float
                , x : Float
                , y : Float
                }
    }


select m_ =
    m_.select
        |> Maybe.map
            (\m ->
                [ Html.div
                    [ style "position" "absolute"
                    , style "top" <| px <| min m.startY m.y
                    , style "width" <| px (abs (m.startX - m.x))
                    , style "height" <| px (abs (m.startY - m.y))
                    , style "left" <| px <| min m.startX m.x
                    , style "background-color" "rgba(64,224,208,.4)"
                    , style "border" "1px solid #00868B"
                    , style "pointer-events" "none"
                    ]
                    []
                ]
            )
        |> Maybe.withDefault []
        |> Html.div
            [ style "position" "absolute"
            , style "top" "0"
            , style "right" "0"
            , style "bottom" "0"
            , style "left" "0"
            , VirtualDom.on "mousedown" handlerMouseDown
            , VirtualDom.on "mouseup" handlerMouseUp
            , VirtualDom.on "mousemove" handlerMouseMove
            ]


px : Float -> String
px a =
    String.fromFloat a ++ "px"


mouseDown : Decoder (Select a -> Select a)
mouseDown =
    D.map2
        (\x y m ->
            { m
                | select =
                    Just
                        { startX = x
                        , startY = y
                        , x = x
                        , y = y
                        }
            }
        )
        (D.field "offsetX" D.float)
        (D.field "offsetY" D.float)


mouseUp : Decoder (Select a -> Select a)
mouseUp =
    D.succeed (\m -> { m | select = Nothing })


mouseMove : Decoder (Select a -> Select a)
mouseMove =
    D.map2
        (\x y m ->
            { m
                | select =
                    m.select
                        |> Maybe.map
                            (\subM ->
                                { subM
                                    | x = x
                                    , y = y
                                }
                            )
            }
        )
        (D.field "offsetX" D.float)
        (D.field "offsetY" D.float)


handlerMouseDown : Handler (Select a -> Select a)
handlerMouseDown =
    MayPreventDefault (D.map (\m -> ( m, False )) mouseDown)


handlerMouseUp : Handler (Select a -> Select a)
handlerMouseUp =
    MayPreventDefault (D.map (\m -> ( m, True )) mouseUp)


handlerMouseMove : Handler (Select a -> Select a)
handlerMouseMove =
    MayPreventDefault (D.map (\m -> ( m, True )) mouseMove)
