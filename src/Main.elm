module Main exposing (main)

import Browser
import Browser.Dom as Browser
import Browser.Events
import Dict exposing (Dict)
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
import Tiled.Level
import Tiled.Tileset
import WebGL.Texture as Texture exposing (Texture)
import WebTiled.Message exposing (Message(..))
import WebTiled.Model as Model exposing (CurrentLevel(..), Images, LevelFrom(..), Model, PropertiesFor(..))
import WebTiled.Panel as Panel2 exposing (preferences)
import WebTiled.Render exposing (textureError, textureOption)
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
            ( model, File.Select.files [] (\a -> (::) a >> ParseFiles) )

        LoadFileFromUrl url ->
            ( model, Http.getLevel url )

        ParseFiles files ->
            ( model, File.getLevel files )

        LoadFileRemoteStorage file ->
            ( model, RS.getFile file )

        ---Save
        Save ->
            case model.level of
                LevelComplete level images tilesets ->
                    let
                        filename =
                            "hello.json"
                    in
                    level
                        |> TiledUtil.sourceTileset
                        |> List.foldl
                            (\{ source, firstgid } acc ->
                                Dict.get source tilesets
                                    |> Maybe.map (\a -> ( source, E.encode 0 (Tiled.Tileset.encode a) ) :: acc)
                                    |> Maybe.withDefault acc
                            )
                            []
                        |> RS.saveLevel filename level (Dict.foldl (\k ( a, b ) -> (::) ( k, a )) [] images)
                        |> Tuple.pair model

                _ ->
                    ( model, Cmd.none )

        --Load
        GotImage name url t ->
            case model.level of
                LevelComplete level images tilesets ->
                    ( model, Cmd.none )

                LevelPartial form level images tilesets inProgress ->
                    ( model, Cmd.none )

                LevelLoading levelForm level images tilesets _ ->
                    let
                        newImages =
                            Dict.insert name ( url, t ) images
                    in
                    case TiledUtil.validate level tilesets newImages of
                        Ok _ ->
                            let
                                render =
                                    WebTiled.Render.initLevel (toRender newImages) (TiledUtil.mergeTileset tilesets level) model.render
                            in
                            ( { model
                                | level = LevelComplete level newImages tilesets
                                , render = render
                              }
                            , Cmd.none
                            )

                        Err inProgress ->
                            ( { model
                                | level = LevelLoading levelForm level newImages tilesets (Set.remove name inProgress)
                              }
                            , Cmd.none
                            )

                LevelNone ->
                    ( model, Cmd.none )

        FileFromUrl ( relUrl, filename ) level tilesets ->
            let
                images =
                    TiledUtil.images level
            in
            ( { model
                | level = LevelLoading (UrlLevel relUrl filename) level Dict.empty tilesets images
              }
            , getImages relUrl level
            )

        FilesFromDisk level tilesets images ->
            if Dict.isEmpty images then
                case TiledUtil.validate level tilesets Dict.empty of
                    Ok _ ->
                        ( { model | level = LevelComplete level Dict.empty tilesets }
                        , Cmd.none
                        )

                    Err missing ->
                        ( { model | level = LevelPartial DiskLevel level Dict.empty tilesets missing }
                        , Cmd.none
                        )

            else
                let
                    needImages =
                        TiledUtil.images level

                    haveImages =
                        Set.fromList (Dict.keys images)

                    missing =
                        Set.diff needImages haveImages
                in
                ( { model
                    | level =
                        if Set.isEmpty missing then
                            LevelLoading DiskLevel level Dict.empty tilesets needImages

                        else
                            LevelPartial DiskLevel level Dict.empty tilesets missing
                  }
                , Cmd.batch <|
                    Dict.foldl
                        (\a b acc ->
                            if Set.member a needImages then
                                getImage a b :: acc

                            else
                                acc
                        )
                        []
                        images
                )

        FileError err ->
            --let
            --    _ =
            --        Debug.log "FileError" err
            --in
            ( model, Cmd.none )

        RemoteStorageFileMissing name ->
            --let
            --    _ =
            --        Debug.log "CANNOT LOAD!::file" name
            --in
            ( model, Cmd.none )

        --Remote Storage
        RemoteStorageFile name contentType dataUrl ->
            case contentType of
                "application/tiled.level-json" ->
                    case D.decodeString Tiled.decode dataUrl of
                        Ok level ->
                            case TiledUtil.validate level Dict.empty Dict.empty of
                                Ok _ ->
                                    let
                                        render =
                                            WebTiled.Render.initLevel Dict.empty level model.render
                                    in
                                    ( { model
                                        | level = LevelComplete level Dict.empty Dict.empty
                                        , render = render
                                      }
                                    , Cmd.none
                                    )

                                Err missingFiles ->
                                    ( { model
                                        | level = LevelLoading RemoteStorageLevel level Dict.empty Dict.empty missingFiles
                                      }
                                    , Set.foldl (RS.getFile >> (::)) [] missingFiles |> Cmd.batch
                                    )

                        _ ->
                            ( model, Cmd.none )

                "image/base64" ->
                    ( model, getImage name dataUrl )

                "application/tiled.tileset-json" ->
                    case model.level of
                        LevelLoading levelForm level images tilesets _ ->
                            let
                                newTilesets =
                                    level
                                        |> TiledUtil.sourceTileset
                                        |> List.find (.source >> (==) name)
                                        |> Maybe.andThen
                                            (\{ firstgid, source } ->
                                                D.decodeString (Tiled.Tileset.decodeFile firstgid) dataUrl
                                                    |> Result.toMaybe
                                                    |> Maybe.map (\t -> Dict.insert source t tilesets)
                                            )
                                        |> Maybe.withDefault tilesets
                            in
                            case TiledUtil.validate level newTilesets images of
                                Ok _ ->
                                    let
                                        render =
                                            WebTiled.Render.initLevel (toRender images) level model.render
                                    in
                                    ( { model
                                        | level = LevelComplete level images newTilesets
                                        , render = render
                                      }
                                    , Cmd.none
                                    )

                                Err inProgress ->
                                    ( { model
                                        | level = LevelLoading levelForm level images newTilesets (Set.remove name inProgress)
                                      }
                                    , Cmd.none
                                    )

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
                LevelComplete level _ _ ->
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
            Panel2.preview2

        right =
            UI.fromList ( Panel2.layers, [{- Panel2.tilesets -}] )

        layout =
            UI.fromList ( Panel2.topMenu, [ Panel2.toolbar, UI.fromList ( left, [ center, right ] ), Panel2.statusbar ] )

        ( m, cmd ) =
            Model.init flags
                |> (case D.decodeValue (D.field "levelUrl" D.string) flags of
                        Ok url ->
                            update (WebTiled.Message.LoadFileFromUrl url)

                        Err _ ->
                            \m_ -> ( m_, Cmd.none )
                   )
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
        |> D.map (\files -> { message = ParseFiles files, stopPropagation = True, preventDefault = True })
        |> Html.Events.custom "contextmenu"


onDrop : Html.Attribute Message
onDrop =
    D.list File.decoder
        |> D.field "files"
        |> D.field "dataTransfer"
        |> D.map (\files -> { message = ParseFiles files, stopPropagation = True, preventDefault = True })
        |> Html.Events.custom "drop"


getImages : String -> Tiled.Level.Level -> Cmd Message
getImages relUrl level =
    TiledUtil.images level
        |> Set.foldl (\name -> getImage name (relUrl ++ name) |> (::)) []
        |> Cmd.batch


getImage : String -> String -> Cmd Message
getImage name url =
    Task.map2 (GotImage name)
        (Http.getBytes url)
        (Texture.loadWith textureOption url |> Task.mapError textureError)
        |> Task.attempt
            (\r ->
                case r of
                    Ok msg ->
                        msg

                    Err err ->
                        FileError err
            )


toRender : Images -> Dict String Texture
toRender =
    Dict.map (\k ( _, t ) -> t)
