module IDE.UI.Widget.Select exposing (view)

import Html exposing (Html, option, select, text)
import Html.Attributes exposing (value)
import Html.Events exposing (on)
import Json.Decode as D


view : Bool -> (String -> msg) -> List Option -> Html msg
view disabled changeMsg options =
    select
        [ onChange changeMsg
        , Html.Attributes.disabled disabled
        ]
        (List.map
            (\opt ->
                option
                    [ value opt.value
                    , Html.Attributes.selected opt.selected
                    , Html.Attributes.disabled opt.disabled
                    ]
                    [ text opt.text ]
            )
            options
        )


type alias Option =
    { text : String
    , value : String
    , selected : Bool
    , disabled : Bool
    }


onChange : (String -> msg) -> Html.Attribute msg
onChange tagger =
    on "change" (D.map tagger Html.Events.targetValue)
