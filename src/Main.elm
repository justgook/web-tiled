module Main exposing (main)

import Browser
import Browser.Dom as Browser
import Browser.Events
import Html exposing (Html)
import Http
import IDE.UI.Html exposing (ResizeInfo)
import IDE.UI.Tree
import Message exposing (Message(..), SucceedData)
import Task
import Tiled
import Tiled.Level exposing (Level)
import WebTiled.PanelTiled as PanelTiled exposing (Kind(..))


type Model
    = Loading
    | Succeed SucceedData Level
    | Fail


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
    Browser.Events.onResize Resize


update : Message -> Model -> ( Model, Cmd Message )
update msg model_ =
    case ( msg, model_ ) of
        ( UI (IDE.UI.Html.Panel msg2), Succeed model level ) ->
            case msg2 of
                PanelTiled.Editor fn ->
                    ( Succeed { model | editor = fn model.editor } level, Cmd.none )

                PanelTiled.Level fn ->
                    ( Succeed model (fn level), Cmd.none )

                PanelTiled.EditorLevel fn ->
                    let
                        ( newEditor, newLevel ) =
                            fn model.editor level
                    in
                    ( Succeed { model | editor = newEditor } newLevel, Cmd.none )

        ( UI msg_, Succeed model level ) ->
            ( Succeed { model | ui2 = IDE.UI.Html.update msg_ model.ui2 } level, Cmd.none )

        ( Resize w h, Succeed model level ) ->
            let
                ui2 =
                    model.ui2
            in
            ( Succeed { model | ui2 = { ui2 | node = IDE.UI.Tree.setSize w h model.ui2.node } } level
            , Cmd.none
            )

        ( Init level model, _ ) ->
            let
                m =
                    case level of
                        Ok level_ ->
                            Succeed model level_

                        Err _ ->
                            Fail
            in
            ( m, Task.perform (\{ scene } -> Resize (round scene.width) (round scene.height)) Browser.getViewport )

        _ ->
            ( model_, Cmd.none )


init : a -> ( Model, Cmd Message )
init _ =
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

        --                |> IDE.UI.Tree.addNorth topToolbar
        url =
            "top-down-adventure/demo.json"

        relUrl =
            String.split "/" url
                |> List.reverse
                |> List.drop 1
                |> (::) ""
                |> List.reverse
                |> String.join "/"

        info =
            { ui2 =
                { node = newNode
                , resizeInfo = Nothing
                }
            , relUrl = relUrl
            , editor = PanelTiled.init
            }

        cmd =
            [ getLevel url info
            ]
                |> Cmd.batch
    in
    ( Loading
    , cmd
    )


getLevel : String -> SucceedData -> Cmd Message
getLevel url info =
    Http.get
        { url = url
        , expect = Http.expectJson (\result -> Init result info) Tiled.decode
        }


view : Model -> Html Message
view model =
    case model of
        Succeed m level ->
            IDE.UI.Html.view (PanelTiled.view m.editor m.relUrl level) m.ui2
                |> Html.map UI

        Loading ->
            Html.span [] []

        Fail ->
            Html.span [] []
