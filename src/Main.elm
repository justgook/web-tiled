module Main exposing (main)

import Browser
import Browser.Dom as Browser
import Browser.Events
import Common exposing (Editor, Message(..), Model)
import Dict
import Html exposing (Html)
import Html.Attributes
import Http
import IDE.UI.Html exposing (ResizeInfo)
import IDE.UI.Tree
import RemoteStorage
import Task
import Tiled
import Tiled.Util as Util
import WebTiled.DropFiles as DropFiles
import WebTiled.PanelTiled as PanelTiled exposing (Kind(..))


main : Program () Model Message
main =
    Browser.document
        { init = init
        , view =
            \model ->
                { title = "WebTiled Editor"
                , body = [ view model ]
                }
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Message
subscriptions _ =
    [ Browser.Events.onResize Resize
    , RemoteStorage.subscriptions
        |> Sub.map (Result.map FromStore >> Result.withDefault FromStoreUnknown)
    ]
        |> Sub.batch


update : Message -> Model -> ( Model, Cmd Message )
update msg ({ editor } as model) =
    case ( msg, model.level ) of
        ( UI (IDE.UI.Html.Custom msg2), Common.Succeed level ) ->
            case msg2 of
                PanelTiled.Editor fn ->
                    ( { model | editor = { editor | editor = fn editor.editor } }
                    , Cmd.none
                    )

                PanelTiled.Level fn ->
                    ( { model | level = Common.Succeed (fn level) }
                    , Cmd.none
                    )

                PanelTiled.EditorLevel fn ->
                    let
                        ( newEditor, newLevel ) =
                            fn editor.editor level
                    in
                    ( { model
                        | level = Common.Succeed newLevel
                        , editor = { editor | editor = newEditor }
                      }
                    , Cmd.none
                    )

                PanelTiled.EditorCmd fn ->
                    let
                        ( newEditor, cmd ) =
                            fn editor.editor
                    in
                    ( { model | editor = { editor | editor = newEditor } }
                    , cmd
                    )

        ( UI msg_, _ ) ->
            ( { model | editor = { editor | ui2 = IDE.UI.Html.update msg_ editor.ui2 } }
            , Cmd.none
            )

        ( Resize w h, _ ) ->
            let
                ui2 =
                    editor.ui2
            in
            ( { model
                | editor = { editor | ui2 = { ui2 | node = IDE.UI.Tree.setSize w h editor.ui2.node } }
              }
            , Cmd.none
            )

        ( Init gotLevel, _ ) ->
            let
                level =
                    case gotLevel of
                        Ok level_ ->
                            Common.Succeed level_

                        Err _ ->
                            Common.Fail "HTTP ERROR"
            in
            ( { model | level = level }
            , [ RemoteStorage.getFiles
              , Browser.getViewport |> Task.perform (\{ scene } -> Resize (round scene.width) (round scene.height))
              ]
                |> Cmd.batch
            )

        ( FilesDropped task, _ ) ->
            ( model, Task.attempt FilesParsed task )

        ( FilesParsed result, current ) ->
            result
                |> Result.toMaybe
                |> Maybe.map
                    (List.foldl
                        (\file ( files, level, cmd ) ->
                            case file of
                                ( name, (DropFiles.Level l) as value, source ) ->
                                    ( Dict.insert name value files
                                    , Common.Succeed l
                                    , RemoteStorage.storeFile "application/tiled.level-json" name source :: cmd
                                    )

                                ( name, (DropFiles.Tileset l) as value, source ) ->
                                    ( Dict.insert name value files
                                    , level
                                    , RemoteStorage.storeFile "application/tiled.tileset-json" name source :: cmd
                                    )

                                ( name, (DropFiles.Image img) as value, _ ) ->
                                    ( Dict.insert name value files
                                    , level
                                    , RemoteStorage.storeFile "image/base64" name img :: cmd
                                    )
                        )
                        ( editor.files, current, [] )
                    )
                |> Maybe.map
                    (\( files, level, cmd ) ->
                        ( { model
                            | level = level
                            , editor =
                                { editor
                                    | files = files
                                    , inStore =
                                        Dict.map (\_ _ -> Nothing) files
                                            |> Dict.union editor.inStore
                                }
                          }
                        , Cmd.batch cmd
                        )
                    )
                |> Maybe.withDefault ( model, Cmd.none )

        ( FromStore fn, current ) ->
            let
                newEditor =
                    fn editor

                level =
                    Dict.diff newEditor.files editor.files
                        |> Dict.foldl
                            (\k v acc ->
                                case v of
                                    DropFiles.Level l ->
                                        --let
                                        --    _ =
                                        --        Util.dependencies l
                                        --            |> Debug.log "FromStore"
                                        --in
                                        Common.Succeed l

                                    _ ->
                                        acc
                            )
                            current
            in
            ( { model
                | editor = newEditor
                , level = level
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


init : a -> ( Model, Cmd Message )
init _ =
    let
        url =
            "top-down-adventure/demo.json"
    in
    ( { level = Common.Loading
      , editor = initEditor url
      }
    , [ getLevel url ] |> Cmd.batch
    )


initEditor url =
    let
        topToolbar =
            IDE.UI.Tree.node MainTools
                |> IDE.UI.Tree.addEast (IDE.UI.Tree.node LayerTools)
                |> IDE.UI.Tree.addEast (IDE.UI.Tree.node ObjectTools)

        leftSide =
            IDE.UI.Tree.node LevelProperties

        rightSide =
            IDE.UI.Tree.node Layers
                |> IDE.UI.Tree.addSouth (IDE.UI.Tree.node Tilesets)

        center =
            IDE.UI.Tree.nodeWith 200 20 Render

        newNode =
            leftSide
                |> IDE.UI.Tree.addEast center
                |> IDE.UI.Tree.addEast rightSide
                |> IDE.UI.Tree.addEast (IDE.UI.Tree.node FileManager)

        --|> IDE.UI.Tree.addNorth topToolbar
        relUrl =
            String.split "/" url
                |> List.reverse
                |> List.drop 1
                |> (::) ""
                |> List.reverse
                |> String.join "/"
    in
    { ui2 =
        { node = newNode
        , resizeInfo = Nothing
        }
    , relUrl = relUrl
    , editor = PanelTiled.init
    , files = Dict.empty
    , inStore = Dict.empty
    }


getLevel : String -> Cmd Message
getLevel url =
    Http.get
        { url = url
        , expect = Http.expectJson (\result -> Init result) Tiled.decode
        }


view : Model -> Html Message
view model =
    case model.level of
        Common.Succeed level ->
            [ IDE.UI.Html.view (PanelTiled.view model.editor level) model.editor.ui2
                |> Html.map UI
            ]
                |> Html.main_ [ Html.Attributes.map FilesDropped DropFiles.onDrop ]

        Common.Loading ->
            Html.span [] [ Html.text "Loading .." ]

        Common.Fail err ->
            Html.span [] [ Html.text <| "FAIL:" ++ err ]
