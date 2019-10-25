module Message exposing (Message(..), SucceedData)

import Http
import IDE.UI.Html exposing (Message(..), ResizeInfo)
import Tiled.Level exposing (Level)
import WebTiled.PanelTiled as PanelTiled exposing (Kind(..))


type Message
    = UI (IDE.UI.Html.Message PanelTiled.Message)
    | Resize Int Int
    | Init (Result Http.Error Level) SucceedData


type alias SucceedData =
    { ui2 : IDE.UI.Html.Model PanelTiled.Kind
    , relUrl : String
    , editor : PanelTiled.Model
    }
