module WebTiled.Panel.Generic exposing (bare, panel)

import Html exposing (..)
import Html.Attributes exposing (..)


panel : Int -> Int -> String -> Html msg -> Html msg
panel w h title content =
    bare w
        h
        [ header [ class "toolbar toolbar-header" ]
            [ h1 [ class "title" ]
                [ text title ]
            ]
        , div
            [ class "pane"
            , style "background" "#BFBFBF"
            , style "display" "flex"
            , style "flex-flow" "column"
            ]
            [ content ]
        ]


bare : Int -> Int -> List (Html msg) -> Html msg
bare w h =
    let
        setWidth width =
            String.fromInt width ++ "px" |> style "width"

        setHeight height =
            String.fromInt height ++ "px" |> style "height"
    in
    div
        [ setWidth w
        , setHeight h
        , style "display" "flex"
        , style "flex-flow" "column"
        , style "position" "relative"

        --, draggable "true"
        ]
