module Main exposing (main)

import Browser
import Browser.Dom as Browser
import Browser.Events
import Dict
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events
import Http
import IDE.UI.Hotkey as Hotkey
import IDE.UI.Html
import IDE.UI.Tree as UI exposing (Tree)
import Init exposing (Editor, Level, Model, initEditor)
import Json.Decode as D
import Message exposing (Message(..))
import RemoteStorage
import Set
import Task
import Tiled
import Tiled.Util as Util
import WebTiled.DropFiles as DropFiles
import WebTiled.Kind exposing (Kind)
import WebTiled.Panel as PanelTiled
import WebTiled.Panel2 as Panel2


main : Program D.Value (Model Message) Message
main =
    Browser.document
        { init = init
        , view = \model -> { title = "WebTiled Editor", body = [ view model ] }
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model Message -> Sub Message
subscriptions model =
    [ Browser.Events.onResize Resize
    , RemoteStorage.subscriptions
        |> Sub.map (Result.map FromStore >> Result.withDefault FromStoreUnknown)
    ]
        |> Sub.batch


update : Message -> Model Message -> ( Model Message, Cmd Message )
update msg ({ editor } as model) =
    case ( msg, model.level ) of
        ( UI2 msg_, _ ) ->
            Panel2.update msg_ model

        ( UI msg2, Init.Succeed level ) ->
            case msg2 of
                PanelTiled.Editor fn ->
                    ( { model | editor = { editor | editor = fn editor.editor } }
                    , Cmd.none
                    )

                PanelTiled.Level fn ->
                    ( { model | level = Init.Succeed (fn level) }
                    , Cmd.none
                    )

                PanelTiled.EditorLevel fn ->
                    let
                        ( newEditor, newLevel ) =
                            fn editor.editor level
                    in
                    ( { model
                        | level = Init.Succeed newLevel
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

        ( Resize w h, _ ) ->
            ( { model
                | editor =
                    { editor
                        | ui = UI.balance w h editor.ui
                    }
                , size = { w = w, h = h }
              }
            , Cmd.none
            )

        ( Init gotLevel, _ ) ->
            let
                level =
                    case gotLevel of
                        Ok level_ ->
                            Init.Succeed level_

                        Err _ ->
                            Init.Fail "HTTP ERROR"
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
                                    , Init.Succeed l
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

                ( level, newCmds ) =
                    Dict.diff newEditor.files editor.files
                        |> Dict.foldl
                            (\k v (( _, cmds ) as acc) ->
                                case v of
                                    DropFiles.Level l ->
                                        ( Init.Succeed l
                                        , Set.diff (Util.dependencies l) (Dict.keys editor.files |> Set.fromList)
                                            |> Set.foldl (RemoteStorage.getFile >> (::)) cmds
                                        )

                                    _ ->
                                        acc
                            )
                            ( current, [] )
            in
            ( { model
                | editor = newEditor
                , level = level
              }
            , Cmd.batch newCmds
            )

        _ ->
            ( model, Cmd.none )


init : D.Value -> ( Model Message, Cmd Message )
init _ =
    let
        url =
            "top-down-adventure/demo.json"

        ui2 =
            Panel2.preferences
    in
    ( { level = Init.Loading
      , editor = initEditor url
      , size = { w = 200, h = 200 }
      , ui2 = ui2
      , state = Panel2.init
      }
    , [ getLevel url ] |> Cmd.batch
    )


getLevel : String -> Cmd Message
getLevel url =
    Http.get
        { url = url
        , expect = Http.expectJson (\result -> Init result) Tiled.decode
        }


view : Model Message -> Html Message
view model =
    case model.level of
        Init.Succeed level ->
            IDE.UI.Html.view (PanelTiled.view model.editor level) model.size.w model.size.h model.editor.ui
                |> (Maybe.map
                        (\modal rest ->
                            rest
                                ++ IDE.UI.Html.modal
                                    (PanelTiled.view model.editor level)
                                    model.size.w
                                    model.size.h
                                    modal
                        )
                        model.editor.modal
                        |> Maybe.withDefault identity
                   )
                |> List.map (Html.map UI)
                |> Html.main_
                    [ Html.Attributes.map FilesDropped DropFiles.onDrop
                    , Html.Attributes.tabindex -1
                    , Html.Events.preventDefaultOn "keydown" (Hotkey.decode model.editor.hotkey |> D.map (\m -> ( m, True )))
                    ]

        Init.Loading ->
            Html.span [ style "font-size" "10em" ] [ Html.text "Loading..." ]

        Init.Fail err ->
            Html.span [] [ Html.text <| "FAIL:" ++ err ]
