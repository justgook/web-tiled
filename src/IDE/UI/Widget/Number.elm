module IDE.UI.Widget.Number exposing (float, int)

--https://www.sketch.com/docs/the-interface/inspector/

import Html exposing (Html, input)
import Html.Attributes exposing (type_, value)
import Html.Events as Event
import IDE.UI.Widget.Internal.Math as Math


type alias FloatModel a =
    { a
        | value : Float
        , string : String
    }


type alias IntModel a =
    { a
        | value : Int
        , string : String
    }


int : IntModel a -> Html (IntModel a -> IntModel a)
int m =
    input
        [ Event.onInput
            (\s m_ ->
                { m_
                    | value = Math.calc s |> Result.map round |> Result.withDefault m_.value
                    , string = s
                }
            )
        , type_ "text"
        , value m.string
        ]
        []


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
        , type_ "text"
        , value m.string
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
