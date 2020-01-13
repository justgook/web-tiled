module Common exposing (Editor, Level(..), Message(..), Model)

import Dict exposing (Dict)
import Http
import IDE.UI2.Tree
import Task exposing (Task)
import Tiled.Level
import WebTiled.DropFiles exposing (DropInfo, File)
import WebTiled.PanelTiled as PanelTiled exposing (Kind(..))


type Message
    = UI (PanelTiled.Message Message)
    | Resize Int Int
    | Init (Result Http.Error Tiled.Level.Level)
    | FilesDropped (Task String (List DropInfo))
    | FilesParsed (Result String (List DropInfo))
      -----
    | FromStore (Editor -> Editor)
    | FromStoreUnknown


type alias Editor =
    { ui3 : IDE.UI2.Tree.Tree PanelTiled.Kind
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
    , size : { w : Int, h : Int }
    }
