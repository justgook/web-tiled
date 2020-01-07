module Message exposing (Editor, Message(..))

import Dict exposing (Dict)
import Http
import IDE.UI.Html exposing (Message(..), ResizeInfo)
import Task exposing (Task)
import Tiled.Level exposing (Level)
import WebTiled.DropFiles exposing (File)
import WebTiled.PanelTiled as PanelTiled exposing (Kind(..))


type Message
    = UI (IDE.UI.Html.Message PanelTiled.Message)
    | Resize Int Int
    | Init (Result Http.Error Level)
    | FilesDropped (Task String (List ( String, File )))
    | FilesParsed (Result String (List ( String, File )))


type alias Editor =
    { ui2 : IDE.UI.Html.Model PanelTiled.Kind
    , relUrl : String
    , editor : PanelTiled.Model
    , files : Dict String File
    }
