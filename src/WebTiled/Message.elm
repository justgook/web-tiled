module WebTiled.Message exposing (Message(..), PreferencesTab(..))

import IDE.UI.Tree exposing (Tree)
import WebTiled.Kind exposing (Kind)


type Message
    = OpenModal (Tree Kind)
    | CloseModal
    | NewMap
    | NewTileset
    | NewWorld
    | Open
    | OpenUrl
    | Save
    | SaveAs
    | Export
    | ExportAs
    | ExportAsImage
    | Reload
    | ShowPreferences PreferencesTab
    | ShowProperties
    | ShowLayers
    | ShowTilesets
    | ShowFileManager
    | ShowToolbar
    | ShowStatusBar
    | FullScreen
    | ClearView
    | ObjectTypesEditor
    | ShowGrid
    | ShowTileAnimations
    | ShowTileCollisionShapes
    | ResizeMap
    | AutoMap
    | AutoMapEditor
    | MapProperties
    | NewTileLayer
    | NewObjectLayer
    | NewImageLayer
    | DuplicateLayer
    | RemoveLayer
    | SelectPreviousLayer
    | SelectNextLayer
    | RaiseLayer
    | LowerLayer
    | LayerProperties


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
