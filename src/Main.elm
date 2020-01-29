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
import Port.BuildRun
import RemoteStorage as RS
import Task
import WebTiled.Message exposing (Message(..))
import WebTiled.Model as Model exposing (CurrentLevel(..), LevelFrom(..), Model, PropertiesFor(..))
import WebTiled.Panel as Panel2 exposing (preferences)
import WebTiled.Util.File as File
import WebTiled.Util.Http as Http


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

    --, RemoteStorage.subscriptions |> Sub.map (Result.map FromStore >> Result.withDefault FromStoreUnknown)
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

        Open ->
            ( model, File.Select.files [] (\a -> (::) a >> GetFiles) )

        GetFileFromUrl url ->
            ( model, Http.getLevel url )

        GetFiles files ->
            ( model, File.getLevel files )

        FileFromUrl relUrl level tilesets ->
            ( { model | level = LevelComplete (UrlLevel relUrl) level (Dict.fromList tilesets) }, Cmd.none )

        FilesFromDisk levels tilesets images ->
            case levels |> Dict.values |> List.head of
                Just level ->
                    case File.validate level tilesets images of
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

        FileError err ->
            --let
            --    _ =
            --        Debug.log "FileError" err
            --in
            ( model, Cmd.none )

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
