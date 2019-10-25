module IDE.UI.Widget.Number exposing (float)

--https://www.sketch.com/docs/the-interface/inspector/

import Html exposing (Html, input)
import Html.Attributes exposing (type_, value)
import Html.Events as Event
import IDE.UI.Widget.Internal.Math as Math


type alias FloatModel a =
    { a
        | value : Float
        , string : String
        , focus : Bool
    }


float : FloatModel a -> Html (FloatModel a -> FloatModel a)
float m =
    input
        [ Event.onInput
            (\s m_ ->
                { m_
                    | value = Math.calc s |> Result.withDefault m_.value
                    , string = s
                }
            )
        , Event.onBlur (\m_ -> { m_ | focus = False })
        , Event.onFocus (\m_ -> { m_ | focus = True })
        , type_ "text"
        , value
            (if m.focus then
                m.string

             else
                formatFloat m.value
            )
        ]
        []


formatFloat : Float -> String
formatFloat f =
    let
        s =
            String.fromFloat f
    in
    if String.contains "." s || String.contains "e" s then
        s

    else
        s ++ ".0"
