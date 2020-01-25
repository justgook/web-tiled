module WebTiled.Message exposing
    ( Message(..)
    , PreferencesTab(..)
    )

import Dict exposing (Dict)
import File exposing (File)
import Tiled.Level
import Tiled.Tileset


type Message
    = NewMap
    | NewWorld
    | Open
    | OpenUrl
    | Save
    | SaveAs
    | Export
    | ExportAs
    | ExportAsImage
    | Reload
    | AutoMap
      --- Global Settings
    | ShowGrid
    | ShowTileAnimations
    | ShowTileCollisionShapes
      --- Screen Mode
    | FullScreen
    | ClearView
      --- Modal
    | AutoMapEditor
    | ShowFileManager
    | ObjectTypesEditor
    | ResizeMap
    | ShowPreferences PreferencesTab
    | CloseModal
      --- Panels
    | ToggleToolbar
    | ToggleStatusBar
    | ToggleTilesets
    | ToggleLayers
    | ToggleProperties
      --- Properties
    | ShowMapProperties
    | ShowLayerProperties
    | ShowTilesetProperties
      --- Layers
    | NewTileLayer
    | NewObjectLayer
    | NewImageLayer
    | DuplicateLayer
    | RemoveLayer
    | SelectPreviousLayer
    | SelectLayer Int
    | SelectNextLayer
    | RaiseLayer
    | LowerLayer
      --- Tilesets
    | NewTileset
    | SelectTileset Int
      --- Files
    | GetFiles (List File)
    | GetFileFromUrl String
    | FileFromUrl String Tiled.Level.Level (List ( Int, Tiled.Tileset.Tileset ))
    | FilesFromDisk (Files Tiled.Level.Level) (Files Tiled.Tileset.Tileset) (Files String)
    | FileError String
      -- RemoteStorage
    | RemoteStorageFile String String String
    | RemoteStorageFileList (List String)
      -- Global Subscription
    | Resize Int Int


type alias Files a =
    Dict String a


type PreferencesTab
    = Account
    | General
    | Appearance
    | Keyboard
    | RadialMenu
    | MobileSettings
    | Publish
    | BuildServer
    | Backup
