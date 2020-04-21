module WebTiled.Model exposing
    ( CurrentLevel(..)
    , Images
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
import WebGL.Texture as WebGL
import WebTiled.Message exposing (PreferencesTab(..))
import WebTiled.Render


type alias Model =
    { selectedLayers : List Int
    , selectedTileset : Int
    , settings : PreferencesTab
    , propertiesFocus : PropertiesFor
    , level : CurrentLevel
    , size : { w : Int, h : Int }
    , modal : Maybe (UI.Layout Kind)
    , layout : UI.Layout Kind
    , render : WebTiled.Render.Model

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
    , render = WebTiled.Render.empty
    , build = { selected = { build = "", run = "", name = "" }, rest = [] }
    , remoteStorage = { status = Connecting, userName = "", files = [] }
    , version = version
    }


type alias RunTemplate =
    { name : String, build : String, run : String }


type LevelFrom
    = UrlLevel String String
    | DiskLevel
    | RemoteStorageLevel


type CurrentLevel
    = LevelComplete Tiled.Level.Level Images Tilesets
    | LevelPartial LevelFrom Tiled.Level.Level Images Tilesets FilesInProgress
    | LevelLoading LevelFrom Tiled.Level.Level Images Tilesets FilesInProgress
    | LevelNone


type alias FilesInProgress =
    Set String


type alias Tilesets =
    Dict String Tiled.Tileset.Tileset


type alias Images =
    Dict String ( String, WebGL.Texture )


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
    | Preview2
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
