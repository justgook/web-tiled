module Main exposing (main)

import Browser
import Browser.Dom as Browser
import Browser.Events
import Dict
import File
import File.Select
import Html exposing (Html)
import Html.Attributes exposing (tabindex)
import Html.Events
import IDE.Internal.List as List
import IDE.UI.Html
import IDE.UI.Layout as UI exposing (Layout)
import Json.Decode as D
import Json.Encode as E
import Port.BuildRun
import RemoteStorage as RS
import Set
import Task
import Tiled
import Tiled.Tileset
import WebTiled.Message exposing (Message(..))
import WebTiled.Model as Model exposing (CurrentLevel(..), LevelFrom(..), Model, PropertiesFor(..))
import WebTiled.Panel as Panel2 exposing (preferences)
import WebTiled.Util.File as File
import WebTiled.Util.Http as Http
import WebTiled.Util.Tiled as TiledUtil


main : Program D.Value Model Message
main =
    Browser.document
        { init = init
        , view = \model -> { title = "WebTiled Editor", body = [ view model ] }
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Message
subscriptions model =
    [ Browser.Events.onResize Resize
    , RS.subscriptions
    ]
        |> Sub.batch


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        SelectLayer i ->
            ( { model | selectedLayers = [ i ], propertiesFocus = LayerProps i }, Cmd.none )

        SelectTileset i ->
            ( { model | selectedTileset = i, propertiesFocus = TilesetProps i }, Cmd.none )

        ShowMapProperties ->
            ( { model | propertiesFocus = LevelProps }, Cmd.none )

        ShowLayerProperties ->
            ( Maybe.map (\i -> { model | propertiesFocus = LayerProps i }) (List.head model.selectedLayers)
                |> Maybe.withDefault model
            , Cmd.none
            )

        ShowTilesetProperties ->
            ( { model | propertiesFocus = TilesetProps model.selectedTileset }, Cmd.none )

        ShowPreferences category ->
            ( { model | settings = category, modal = Just preferences }, Cmd.none )

        CloseModal ->
            ( { model | modal = Nothing }, Cmd.none )

        --- File
        Open ->
            ( model, File.Select.files [] (\a -> (::) a >> GetFiles) )

        GetFileFromUrl url ->
            ( model, Http.getLevel url )

        GetFiles files ->
            ( model, File.getLevel files )

        GetFileRemoteStorage file ->
            ( model, RS.getFile file )

        FileFromUrl ( relUrl, filename ) level tilesets ->
            ( { model | level = LevelComplete (UrlLevel relUrl filename) level (Dict.fromList tilesets) }, Cmd.none )

        FilesFromDisk levels tilesets images ->
            case levels |> Dict.values |> List.head of
                Just level ->
                    case TiledUtil.validate level tilesets images of
                        Ok _ ->
                            ( { model | level = LevelComplete (DiskLevel images) level Dict.empty }, Cmd.none )

                        Err err ->
                            --let
                            --    _ =
                            --        Debug.log "FilesFromDisk-ERROR" err
                            --in
                            ( model, Cmd.none )

                Nothing ->
                    --let
                    --    _ =
                    --        Debug.log "FilesFromDisk-ERROR" "NO LEVEL"
                    --in
                    ( model, Cmd.none )

        SaveFromUrl result ->
            case ( result, model.level ) of
                ( Ok images, LevelComplete (UrlLevel _ filename) level tilesets ) ->
                    level
                        |> TiledUtil.sourceTileset
                        |> List.foldl
                            (\{ source, firstgid } acc ->
                                Dict.get firstgid tilesets
                                    |> Maybe.map (\a -> ( source, E.encode 0 (Tiled.Tileset.encode a) ) :: acc)
                                    |> Maybe.withDefault acc
                            )
                            []
                        |> RS.saveLevel ( filename, level ) images
                        |> Tuple.pair model

                _ ->
                    ( model, Cmd.none )

        Save ->
            case model.level of
                LevelComplete (UrlLevel url _) level _ ->
                    ( model
                    , level
                        |> TiledUtil.images
                        |> Set.foldl (\name -> Http.getBytes (url ++ name) |> Task.map (Tuple.pair name) |> (::)) []
                        |> Task.sequence
                        |> Task.attempt SaveFromUrl
                    )

                _ ->
                    ( model, Cmd.none )

        FileError err ->
            --let
            --    _ =
            --        Debug.log "FileError" err
            --in
            ( model, Cmd.none )

        FileMissing name ->
            --let
            --    _ =
            --        Debug.log "CANNOT LOAD!::file" name
            --in
            ( model, Cmd.none )

        --Remote Storage
        RemoteStorageFile name contentType data ->
            case contentType of
                "application/tiled.level-json" ->
                    case D.decodeString Tiled.decode data of
                        Ok level ->
                            case TiledUtil.validate level Dict.empty Dict.empty of
                                Ok _ ->
                                    ( { model
                                        | level =
                                            LevelComplete
                                                (RemoteStorageLevel Dict.empty)
                                                level
                                                Dict.empty
                                      }
                                    , Cmd.none
                                    )

                                Err missingFiles ->
                                    ( { model
                                        | level =
                                            LevelLoading
                                                (RemoteStorageLevel Dict.empty)
                                                level
                                                Dict.empty
                                                Dict.empty
                                                missingFiles
                                      }
                                    , Set.foldl (RS.getFile >> (::)) [] missingFiles |> Cmd.batch
                                    )

                        _ ->
                            ( model, Cmd.none )

                "image/base64" ->
                    case model.level of
                        LevelLoading levelForm level tilesets images _ ->
                            let
                                newImages =
                                    Dict.insert name data images
                            in
                            case TiledUtil.validate level tilesets newImages of
                                Ok _ ->
                                    ( { model
                                        | level =
                                            LevelComplete
                                                (RemoteStorageLevel newImages)
                                                level
                                                Dict.empty
                                      }
                                    , Cmd.none
                                    )

                                Err inProgress ->
                                    ( { model
                                        | level = LevelLoading levelForm level tilesets newImages (Set.remove name inProgress)
                                      }
                                    , Cmd.none
                                    )

                        _ ->
                            ( model, Cmd.none )

                "application/tiled.tileset-json" ->
                    case model.level of
                        LevelLoading levelForm level tilesets images _ ->
                            let
                                newTilesets =
                                    level
                                        |> TiledUtil.sourceTileset
                                        |> List.find (.source >> (==) name)
                                        |> Maybe.andThen
                                            (\{ firstgid, source } ->
                                                D.decodeString (Tiled.Tileset.decodeFile firstgid) data
                                                    |> Result.toMaybe
                                                    |> Maybe.map (\t -> Dict.insert source t tilesets)
                                            )
                                        |> Maybe.withDefault tilesets
                            in
                            case TiledUtil.validate level newTilesets images of
                                Ok _ ->
                                    ( { model
                                        | level =
                                            LevelComplete
                                                (RemoteStorageLevel images)
                                                level
                                                (Dict.foldl (\_ v -> Dict.insert (TiledUtil.firstGid v) v) Dict.empty newTilesets)
                                      }
                                    , Cmd.none
                                    )

                                Err inProgress ->
                                    ( { model
                                        | level = LevelLoading levelForm level newTilesets images (Set.remove name inProgress)
                                      }
                                    , Cmd.none
                                    )

                        --
                        _ ->
                            ( model, Cmd.none )

                _ ->
                    --let
                    --    _ =
                    --        Debug.log "RemoteStorageFile" ( name, contentType )
                    --in
                    ( model, Cmd.none )

        RemoteStorageUserNameChange userName ->
            let
                remoteStorage =
                    model.remoteStorage
            in
            ( { model | remoteStorage = { remoteStorage | userName = userName } }, Cmd.none )

        RemoteStorageConnect ->
            let
                remoteStorage =
                    model.remoteStorage
            in
            ( { model | remoteStorage = { remoteStorage | status = Model.Connecting } }, RS.connect remoteStorage.userName )

        RemoteStorageDisconnect ->
            let
                remoteStorage =
                    model.remoteStorage
            in
            ( { model | remoteStorage = { remoteStorage | status = Model.Connecting } }, RS.disconnect )

        RemoteStorageOffline ->
            let
                remoteStorage =
                    model.remoteStorage
            in
            ( { model | remoteStorage = { remoteStorage | status = Model.Offline } }, Cmd.none )

        RemoteStorageOnline userAddress ->
            let
                remoteStorage =
                    model.remoteStorage
            in
            ( { model | remoteStorage = { remoteStorage | status = Model.Online, userName = userAddress } }, Cmd.none )

        RemoteStorageSyncing ->
            let
                remoteStorage =
                    model.remoteStorage
            in
            ( { model | remoteStorage = { remoteStorage | status = Model.Syncing } }, Cmd.none )

        RemoteStorageSyncDone ->
            let
                remoteStorage =
                    model.remoteStorage
            in
            ( { model | remoteStorage = { remoteStorage | status = Model.Online } }, Cmd.none )

        RemoteStorageFileList files ->
            let
                remoteStorage =
                    model.remoteStorage
            in
            ( { model | remoteStorage = { remoteStorage | files = files } }, Cmd.none )

        RemoteStorageUnhandledEvent key ->
            --let
            --    _ =
            --        Debug.log "RemoteStorageUnhandledEvent" key
            --in
            ( model, Cmd.none )

        ---Scripts
        Run ->
            case model.level of
                LevelComplete _ level _ ->
                    ( model, Port.BuildRun.levelBuild model.build.selected level )

                _ ->
                    ( model, Cmd.none )

        SetRunScript i ->
            let
                build =
                    model.build
            in
            ( { model
                | build =
                    build.rest
                        |> List.getAt i
                        |> Maybe.map (\item -> { build | selected = item })
                        |> Maybe.withDefault build
              }
            , Cmd.none
            )

        Resize w h ->
            ( { model
                | layout = UI.balance w h model.layout
                , size = { w = w, h = h }
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


init : D.Value -> ( Model, Cmd Message )
init flags =
    let
        url =
            D.decodeValue (D.field "levelUrl" D.string) flags
                |> Result.withDefault "top-down-adventure/demo.json"

        runScripts =
            { selected =
                { build = "https://justgook.github.io/new-age/JumpGun/build.js"
                , run = "https://justgook.github.io/new-age/JumpGun/game.js"
                , name = "Lut builder"
                }
            , rest =
                [ { build = "https://justgook.github.io/new-age/JumpGun/build.js"
                  , run = "https://justgook.github.io/new-age/JumpGun/game.js"
                  , name = "Lut builder"
                  }

                --, { build = "http://localhost:8002/bundle.js"
                --  , run = "http://localhost:8003/game.js"
                --  , name = "RPG"
                --  }
                --, { build = "http://localhost:8002/bundle.js"
                --  , run = "http://localhost:8003/game.js"
                --  , name = "SHMUP"
                --  }
                ]
            }

        left =
            UI.fromList ( Panel2.properties, [ Panel2.fileManager ] )

        center =
            Panel2.preview

        right =
            UI.fromList ( Panel2.layers, [ Panel2.tilesets ] )

        layout =
            UI.fromList ( Panel2.topMenu, [ Panel2.toolbar, UI.fromList ( left, [ center, right ] ), Panel2.statusbar ] )

        ( m, cmd ) =
            Model.init flags
                |> update (WebTiled.Message.GetFileFromUrl url)
    in
    ( { m
        | layout = layout
        , build = runScripts
      }
    , [ cmd
      , RS.getFiles
      , Browser.getViewport |> Task.perform (\{ scene } -> Resize (round scene.width) (round scene.height))
      ]
        |> Cmd.batch
    )


view : Model -> Html Message
view model =
    IDE.UI.Html.view (Panel2.render model) model.size.w model.size.h model.layout
        |> (Maybe.map
                (\modal rest ->
                    rest ++ IDE.UI.Html.modal (Panel2.render model) model.size.w model.size.h modal
                )
                model.modal
                |> Maybe.withDefault identity
           )
        |> Html.main_ [ tabindex -1, onDrop ]



--[
--, Html.Events.preventDefaultOn "keydown" (Hotkey.decode model.editor.hotkey |> D.map (\m -> ( m, True )))
--]


contextmenu =
    D.list File.decoder
        |> D.field "files"
        |> D.field "dataTransfer"
        |> D.map (\files -> { message = GetFiles files, stopPropagation = True, preventDefault = True })
        |> Html.Events.custom "contextmenu"


onDrop : Html.Attribute Message
onDrop =
    D.list File.decoder
        |> D.field "files"
        |> D.field "dataTransfer"
        |> D.map (\files -> { message = GetFiles files, stopPropagation = True, preventDefault = True })
        |> Html.Events.custom "drop"
