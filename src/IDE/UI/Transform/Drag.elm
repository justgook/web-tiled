module IDE.UI.Transform.Drag exposing (Model)

import Json.Decode as D


type alias Model a =
    { a
        | drag : { x : Float, y : Float }
    }



--https://stackblitz.com/edit/multi-touch-trackpad-gesture


decoder =
    D.field



--(Decoder
--              { message : msg
--              , stopPropagation : Bool
--              , preventDefault : Bool
--              }
--          )
