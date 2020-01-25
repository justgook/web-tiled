module WebTiled.Panel.Progress exposing (panel, view)

import Html exposing (div, text)
import Html.Attributes exposing (attribute, class, style)
import IDE.UI.Layout as Layout
import WebTiled.Model exposing (Kind(..))


view : Html.Html msg
view =
    div [ class "loading", attribute "data-progress" "0" ] []


panel : Layout.Layout Kind
panel =
    Layout.node FakeProgress |> Layout.setLimits { yMax = Just 230, yMin = 220, xMax = Just 220, xMin = 220 }
