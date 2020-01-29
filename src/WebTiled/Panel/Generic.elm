module WebTiled.Panel.Generic exposing (bare, panel)

import Html exposing (..)
import Html.Attributes exposing (..)


panel : Int -> Int -> Int -> Int -> String -> Html msg -> Html msg
panel x y w h title content =
    bare x
        y
        w
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


bare : Int -> Int -> Int -> Int -> List (Html msg) -> Html msg
bare x y w h =
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
        , style "width" <| px w
        , style "height" <| px h
        , style "top" <| px y
        , style "left" <| px x
        , style "position" "absolute"

        --, draggable "true"
        ]


px : Int -> String
px i =
    String.fromInt i ++ "px"
