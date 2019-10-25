module IDE.UI.Widget.Input exposing (input)

import Html.Attributes exposing (type_, value)


input m =
    input [ type_ "text", value m.string ] []
