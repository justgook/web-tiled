module IDE.UI.Transform.DragScale exposing (DragScale, apply, handlerWheel, wheel)

import Html exposing (Attribute)
import Html.Attributes as Html exposing (attribute)
import Json.Decode as D exposing (Decoder)
import VirtualDom exposing (Handler(..))


type alias DragScale a =
    { a
        | scale : Float
        , drag : { x : Float, y : Float }
    }



--https://stackblitz.com/edit/multi-touch-trackpad-gesture


apply : DragScale a -> Attribute msg
apply m =
    let
        x =
            String.fromFloat m.drag.x

        y =
            String.fromFloat m.drag.y

        s =
            String.fromFloat (max 0 m.scale)
    in
    ("transform-origin: 0 0;transform:matrix(" ++ s ++ " , 0, 0, " ++ s ++ ", " ++ x ++ ", " ++ y ++ ")")
        |> attribute "style"


wheel : Decoder (DragScale a -> DragScale a)
wheel =
    D.map6
        (\ctrlKey deltaX deltaY layerX layerY platform model ->
            if ctrlKey then
                let
                    s =
                        model.scale * deltaY * 0.01

                    relX =
                        (layerX - model.drag.x) / model.scale

                    relY =
                        (layerY - model.drag.y) / model.scale
                in
                { model
                    | scale = model.scale - s
                    , drag =
                        { x = model.drag.x + (relX * s)
                        , y = model.drag.y + (relY * s)
                        }
                }

            else
                { model
                    | drag =
                        { x = model.drag.x - deltaX
                        , y = model.drag.y - deltaY
                        }
                }
        )
        (D.field "ctrlKey" D.bool)
        (D.field "deltaX" D.float)
        (D.field "deltaY" D.float)
        (D.field "layerX" D.float)
        (D.field "layerY" D.float)
        (D.maybe (D.at [ "view", "navigator", "platform" ] D.string))


handlerWheel : Handler (DragScale a -> DragScale a)
handlerWheel =
    MayPreventDefault (D.map (\m -> ( m, True )) wheel)
