module Message exposing (Message(..))

import Http
import Init exposing (Editor)
import Task exposing (Task)
import Tiled.Level
import WebTiled.DropFiles exposing (DropInfo, File)
import WebTiled.Message
import WebTiled.Panel as Panel


type Message
    = UI (Panel.Message Message)
    | Resize Int Int
    | Init (Result Http.Error Tiled.Level.Level)
    | Action String
    | FilesDropped (Task String (List DropInfo))
    | FilesParsed (Result String (List DropInfo))
      -----
    | FromStore (Editor Message -> Editor Message)
    | FromStoreUnknown
      ----------------------------
    | UI2 WebTiled.Message.Message
