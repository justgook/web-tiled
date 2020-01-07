module Main exposing (main)

import Browser
import Browser.Dom as Browser
import Browser.Events
import Dict
import Html exposing (Html)
import Html.Attributes
import Http
import IDE.UI.Html exposing (ResizeInfo)
import IDE.UI.Tree
import Message exposing (Editor, Message(..))
import Task
import Tiled
import Tiled.Level
import WebTiled.DropFiles as DropFiles
import WebTiled.PanelTiled as PanelTiled exposing (Kind(..))


type Level
    = Loading
    | Succeed Tiled.Level.Level
    | Fail String


type alias Model_ =
    { level : Level
    , editor : Editor
    }


main : Program () Model_ Message
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


subscriptions : Model_ -> Sub Message
subscriptions _ =
    Browser.Events.onResize Resize


update : Message -> Model_ -> ( Model_, Cmd Message )
update msg ({ editor } as model) =
    case ( msg, model.level ) of
        ( UI (IDE.UI.Html.Custom msg2), Succeed level ) ->
            case msg2 of
                PanelTiled.Editor fn ->
                    ( { model | editor = { editor | editor = fn editor.editor } }
                    , Cmd.none
                    )

                PanelTiled.Level fn ->
                    ( { model | level = Succeed (fn level) }
                    , Cmd.none
                    )

                PanelTiled.EditorLevel fn ->
                    let
                        ( newEditor, newLevel ) =
                            fn editor.editor level
                    in
                    ( { model
                        | level = Succeed newLevel
                        , editor = { editor | editor = newEditor }
                      }
                    , Cmd.none
                    )

        ( UI msg_, Succeed level ) ->
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
                            Succeed level_

                        Err _ ->
                            Fail "HTTP ERROR"
            in
            ( { model | level = level }
            , Task.perform
                (\{ scene } ->
                    Resize (round scene.width) (round scene.height)
                )
                Browser.getViewport
            )

        ( FilesDropped task, Succeed level ) ->
            ( model, Task.attempt FilesParsed task )

        ( FilesParsed result, current ) ->
            let
                _ =
                    Debug.log "FilesParsed" "result"
            in
            result
                |> Result.toMaybe
                |> Maybe.map
                    (List.foldl
                        (\file ( files, level ) ->
                            case file of
                                ( name, DropFiles.Level l ) ->
                                    ( files, Succeed l )

                                ( name, value ) ->
                                    ( Dict.insert name value files, level )
                        )
                        ( editor.files, current )
                    )
                |> Maybe.map
                    (\( files, level ) ->
                        ( { model
                            | level = level
                            , editor = { editor | files = files }
                          }
                        , Cmd.none
                        )
                    )
                |> Maybe.withDefault ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


init : a -> ( Model_, Cmd Message )
init _ =
    let
        url =
            "top-down-adventure/demo.json"
    in
    ( { level = Loading
      , editor = initEditor url
      }
    , getLevel url
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
    }


getLevel : String -> Cmd Message
getLevel url =
    Http.get
        { url = url
        , expect = Http.expectJson (\result -> Init result) Tiled.decode
        }


view : Model_ -> Html Message
view model =
    case model.level of
        Succeed level ->
            [ IDE.UI.Html.view (PanelTiled.view model.editor level) model.editor.ui2
                |> Html.map UI
            ]
                |> Html.main_ [ Html.Attributes.map FilesDropped DropFiles.onDrop ]

        Loading ->
            Html.span [] [ Html.text "Loading .." ]

        Fail err ->
            Html.span [] [ Html.text <| "FAIL:" ++ err ]
