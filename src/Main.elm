module Main exposing (main)

import Browser
import Element
import Element.Background as Background
import Element.Font as Font
import WebTiled.Cell as Cell
import WebTiled.Theme as Theme


type Message
    = UI Cell.Message


type alias Model =
    { ui : Cell.Model
    }


main : Program () Model Message
main =
    Browser.document
        { init = init
        , view =
            \model ->
                { title = "String"
                , body = [ view Theme.colors model ]
                }
        , update = update
        , subscriptions = \model -> Sub.none
        }


update msg model =
    case msg of
        UI msg_ ->
            ( { model | ui = Cell.update msg_ model.ui }, Cmd.none )


init _ =
    ( { ui = Cell.empty }, Cmd.none )


view theme model =
    Cell.view theme model.ui
        |> Element.map UI
        |> Element.layout
            [ Font.color theme.font
            , Background.color theme.background
            ]
