module Common exposing (Editor, Level(..), Message(..), Model)

import Dict exposing (Dict)
import Http
import IDE.UI.Html exposing (Message(..), ResizeInfo)
import Task exposing (Task)
import Tiled.Level
import WebTiled.DropFiles exposing (DropInfo, File)
import WebTiled.PanelTiled as PanelTiled exposing (Kind(..))


type Message
    = UI (IDE.UI.Html.Message (PanelTiled.Message Message))
    | Resize Int Int
    | Init (Result Http.Error Tiled.Level.Level)
    | FilesDropped (Task String (List DropInfo))
    | FilesParsed (Result String (List DropInfo))
      -----
    | FromStore (Editor -> Editor)
    | FromStoreUnknown


type alias Editor =
    { ui2 : IDE.UI.Html.Model PanelTiled.Kind
    , relUrl : String
    , editor : PanelTiled.Model
    , files : Dict String File
    , inStore : Dict String (Maybe String)
    }


type Level
    = Loading
    | Succeed Tiled.Level.Level
    | Fail String


type alias Model =
    { level : Level
    , editor : Editor
    }
