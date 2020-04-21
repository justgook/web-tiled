module WebTiled.Message exposing
    ( Message(..)
    , PreferencesTab(..)
    )

import Dict exposing (Dict)
import File exposing (File)
import Playground
import Tiled.Level
import Tiled.Tileset
import WebGL.Texture as WebGL


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
    | Run
    | SetRunScript Int
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
      --- Files Request
    | ParseFiles (List File)
    | LoadFileFromUrl String
    | LoadFileRemoteStorage String
      --- Files Response
    | FileFromUrl ( String, String ) Tiled.Level.Level (Dict String Tiled.Tileset.Tileset)
    | FilesFromDisk Tiled.Level.Level (Files Tiled.Tileset.Tileset) (Files String)
    | GotImage String String WebGL.Texture
    | FileError String
      -- RemoteStorage
    | RemoteStorageFile String String String
    | RemoteStorageFileList (List String)
    | RemoteStorageFileMissing String
    | RemoteStorageOffline
    | RemoteStorageSyncing
    | RemoteStorageSyncDone
    | RemoteStorageOnline String
    | RemoteStorageUserNameChange String
    | RemoteStorageConnect
    | RemoteStorageDisconnect
    | RemoteStorageUnhandledEvent String
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
