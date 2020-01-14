module Main exposing (main)

import Browser
import Browser.Dom as Browser
import Browser.Events
import Common exposing (Editor, Message(..), Model)
import Dict
import Html exposing (Html)
import Html.Attributes
import Http
import IDE.UI2.Html
import IDE.UI2.Tree exposing (getLimitsV)
import RemoteStorage
import Set
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
        ( UI msg2, Common.Succeed level ) ->
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

        ( Resize w h, _ ) ->
            ( { model
                | editor =
                    { editor
                        | ui3 = IDE.UI2.Tree.balance w h editor.ui3
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

                ( level, newCmds ) =
                    Dict.diff newEditor.files editor.files
                        |> Dict.foldl
                            (\k v (( _, cmds ) as acc) ->
                                case v of
                                    DropFiles.Level l ->
                                        ( Common.Succeed l
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


init : a -> ( Model, Cmd Message )
init _ =
    let
        url =
            "top-down-adventure/demo.json"
    in
    ( { level = Common.Loading
      , editor = initEditor url
      , size = { w = 200, h = 200 }
      }
    , [ getLevel url ] |> Cmd.batch
    )


initEditor url =
    let
        relUrl =
            String.split "/" url
                |> List.reverse
                |> List.drop 1
                |> (::) ""
                |> List.reverse
                |> String.join "/"

        -----------------------------------
        topToolbar2 =
            IDE.UI2.Tree.fromList ( PanelTiled.block.mainTools, [ PanelTiled.block.layerTools ] )

        leftSide2 =
            IDE.UI2.Tree.fromList ( PanelTiled.block.levelProperties, [ PanelTiled.block.properties ] )

        center2 =
            IDE.UI2.Tree.fromList ( PanelTiled.block.render, [] )

        rightSide2 =
            IDE.UI2.Tree.fromList ( PanelTiled.block.layers, [ PanelTiled.block.tilesets ] )

        mainStuff =
            IDE.UI2.Tree.fromList ( leftSide2, [ center2, PanelTiled.block.fileManager, rightSide2 ] )

        allTogether =
            IDE.UI2.Tree.fromList ( topToolbar2, [ mainStuff ] )
    in
    { ui3 = allTogether
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
            [ IDE.UI2.Html.view (PanelTiled.view model.editor level) model.size.w model.size.h model.editor.ui3
                |> Html.map UI
            ]
                |> Html.main_ [ Html.Attributes.map FilesDropped DropFiles.onDrop ]

        Common.Loading ->
            Html.span [] [ Html.text "Loading .." ]

        Common.Fail err ->
            Html.span [] [ Html.text <| "FAIL:" ++ err ]
