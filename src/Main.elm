module Main exposing (main)

import Browser
import Browser.Dom as Browser
import Browser.Events
import Html exposing (Html, div)
import Http
import Task
import Tiled
import Tiled.Level exposing (Level)
import WebTiled.PanelTiled as Panel exposing (Model(..))
import WebTiled.UI.Html exposing (Message(..), ResizeInfo)
import WebTiled.UI.Tree


type Message
    = UI WebTiled.UI.Html.Message
    | Resize Int Int
    | Init (Result Http.Error Level) SucceedData


type Model
    = Loading
    | Succeed SucceedData Level
    | Fail


type alias SucceedData =
    { ui2 : WebTiled.UI.Html.Model Panel.Model
    , data : List Int
    , relUrl : String
    }


type alias Panel =
    { title : String }


main : Program () Model Message
main =
    Browser.document
        { init = init
        , view =
            \model ->
                { title = "Application Title"
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
        ( UI msg_, Succeed model level ) ->
            ( Succeed { model | ui2 = WebTiled.UI.Html.update msg_ model.ui2 } level, Cmd.none )

        ( Resize w h, Succeed model level ) ->
            let
                ui2 =
                    model.ui2
            in
            ( Succeed { model | ui2 = { ui2 | node = WebTiled.UI.Tree.setSize w h model.ui2.node } } level
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
            WebTiled.UI.Tree.node MainTools
                |> WebTiled.UI.Tree.addEast (WebTiled.UI.Tree.node LayerTools)
                |> WebTiled.UI.Tree.addEast (WebTiled.UI.Tree.node ObjectTools)

        leftSide =
            WebTiled.UI.Tree.node LevelProperties

        rightSide =
            WebTiled.UI.Tree.node Layers
                |> WebTiled.UI.Tree.addSouth (WebTiled.UI.Tree.node Tilesets)

        center =
            WebTiled.UI.Tree.node Render

        newNode =
            leftSide
                |> WebTiled.UI.Tree.addEast center
                |> WebTiled.UI.Tree.addEast rightSide
                |> WebTiled.UI.Tree.addNorth topToolbar

        url =
            "TopDown/demo.json"

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
            , data = []
            , relUrl = relUrl
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
            WebTiled.UI.Html.view (Panel.view m.relUrl level) m.ui2
                |> Html.map UI

        Loading ->
            Html.span [] []

        Fail ->
            Html.span [] []
