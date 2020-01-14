module IDE.UI2.Html exposing (view)

import Html exposing (..)
import Html.Attributes exposing (class, style)
import IDE.Internal.Many as Many exposing (Many)
import IDE.UI2.Tree exposing (Tree(..))


view : (Int -> Int -> panel -> Html msg) -> Int -> Int -> Tree panel -> List (Html msg)
view fn w h m =
    viewV fn w h m
        |> (::) (Html.node "style" [] [ text css ])


modal fn w h m =
    div [ class "modal" ] []


viewH fn w h_ m =
    case m of
        Branch p childs ->
            let
                h =
                    apply p h_
            in
            [ div
                [ style "width" <| px w
                , style "height" <| px h
                , class "hPanel"
                ]
                (Many.foldl (viewV fn w h >> flip (++)) [] childs)
            ]

        Leaf p _ kind ->
            let
                h =
                    apply p h_
            in
            [ div
                [ style "width" <| px w
                , style "height" <| px h
                , class "content"
                ]
                [ fn w h kind ]
            ]


viewV fn w_ h m =
    case m of
        Branch p childs ->
            let
                w =
                    apply p w_
            in
            [ div
                [ style "width" <| px w
                , style "height" <| px h
                , class "vPanel"
                ]
                (Many.foldl (viewH fn w h >> flip (++)) [] childs)
            ]

        Leaf p _ kind ->
            let
                w =
                    apply p w_
            in
            [ div
                [ style "width" <| px w
                , style "height" <| px h
                , class "content"
                ]
                [ fn w h kind ]
            ]


flip fn a b =
    fn b a


px : Int -> String
px i =
    String.fromInt i ++ "px"


apply p h =
    h |> toFloat |> (*) p |> floor


css =
    """
.vPanel {
    float:left;
    position:relative;
}

.hPanel {
    float:left;
    position:relative;
}

.content {
    float:left;
    position:relative;
}

"""
