module WebTiled.Model exposing
    ( CurrentLevel(..)
    , Kind(..)
    , LevelFrom(..)
    , Model
    , PropertiesFor(..)
    , RemoteStorageStatus(..)
    , init
    )

import Dict exposing (Dict)
import IDE.UI.Layout as UI
import Json.Decode as D
import Set exposing (Set)
import Tiled.Level
import Tiled.Tileset
import WebTiled.Message exposing (PreferencesTab(..))


type alias Model =
    { selectedLayers : List Int
    , selectedTileset : Int
    , settings : PreferencesTab
    , propertiesFocus : PropertiesFor
    , level : CurrentLevel
    , size : { w : Int, h : Int }
    , modal : Maybe (UI.Layout Kind)
    , layout : UI.Layout Kind

    --- Running
    , build :
        { selected : RunTemplate
        , rest : List RunTemplate
        }

    --, files : Dict String ParsedFile
    --, inStore : Dict String (Maybe String)
    ----
    , remoteStorage : RemoteStorage
    , version : String
    }


init : D.Value -> Model
init flags =
    let
        version =
            D.decodeValue (D.field "version" D.string) flags
                |> Result.withDefault "1.0.0"
    in
    { selectedLayers = [ 0 ]
    , selectedTileset = 0
    , settings = Account
    , propertiesFocus = LevelProps
    , level = LevelNone
    , size = { w = 800, h = 600 }
    , modal = Nothing --Just (UI.node FakeProgress |> UI.setLimits { yMax = Just 200, yMin = 200, xMax = Just 200, xMin = 200 })
    , layout = UI.node Error
    , build = { selected = { build = "", run = "", name = "" }, rest = [] }
    , remoteStorage = { status = Connecting, userName = "", files = [] }
    , version = version
    }


type alias RunTemplate =
    { name : String, build : String, run : String }


type LevelFrom
    = UrlLevel String String
    | DiskLevel Images
    | RemoteStorageLevel Images


type CurrentLevel
    = LevelComplete LevelFrom Tiled.Level.Level (Dict Int Tiled.Tileset.Tileset)
    | LevelPartial LevelFrom Tiled.Level.Level (Dict String Tiled.Tileset.Tileset) (List String)
    | LevelLoading LevelFrom Tiled.Level.Level (Dict String Tiled.Tileset.Tileset) Images FilesInProgress
    | LevelNone


type alias FilesInProgress =
    Set String


type alias Images =
    Dict String String


type PropertiesFor
    = TilesetProps Int
    | TileProps Int
    | LayerProps Int
    | ObjectProps Int
    | LevelProps


type Kind
    = MainTools
    | RunTools
    | TileLayerTools
    | CloudTools
    | Statusbar
    | ObjectTools
    | Properties
    | Layers
    | Tilesets
    | Preview
    | FileManager
    | TopMenu
    | Preferences
    | FakeProgress
    | Error


type alias RemoteStorage =
    { status : RemoteStorageStatus
    , files : List String
    , userName : String
    }


type RemoteStorageStatus
    = Offline
    | Connecting
    | Online
    | Syncing
