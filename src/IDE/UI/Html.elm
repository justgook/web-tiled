module IDE.UI.Html exposing (modal, view)

import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Lazy
import IDE.Internal.Many as Many exposing (Many)
import IDE.UI.Layout as Tree exposing (Layout(..))


view : (Int -> Int -> panel -> Html msg) -> Int -> Int -> Layout panel -> List (Html msg)
view fn w h m =
    wrapper fn w h m
        |> (::) (Html.Lazy.lazy css ())


modal : (Int -> Int -> panel -> Html msg) -> Int -> Int -> Layout panel -> List (Html msg)
modal fn w_ h_ m =
    let
        { xMax, yMax, xMin, yMin } =
            Tree.getLimitsV m

        w =
            Maybe.map (min (w_ // 4 * 3)) xMax
                |> Maybe.withDefault (max xMin (w_ // 4 * 3))

        h =
            Maybe.map (min (h_ // 4 * 3)) yMax
                |> Maybe.withDefault (max yMin (h_ // 4 * 3))
    in
    [ Html.Lazy.lazy modalCss ()
    , div [ class "modal-background" ] []
    , div
        [ class "modal"
        , style "width" <| px w
        , style "height" <| px h
        ]
        (wrapper fn w h m)
    ]


wrapper : (Int -> Int -> panel -> Html msg) -> Int -> Int -> Layout panel -> List (Html msg)
wrapper fn w h m =
    let
        pxW p a =
            a |> toFloat |> (*) p |> floor

        pxH _ a =
            a
    in
    wrapper_ fn pxW pxH w h m


wrapper_ : (Int -> Int -> panel -> Html msg) -> (Tree.Size -> Int -> Int) -> (Tree.Size -> Int -> Int) -> Int -> Int -> Layout panel -> List (Html msg)
wrapper_ fn pxW pxH w_ h_ m =
    case m of
        Branch p childs ->
            let
                w =
                    pxW p w_

                h =
                    pxH p h_
            in
            [ div
                [ style "width" <| px w
                , style "height" <| px h
                , class "panels"
                ]
                (Many.foldl (wrapper_ fn pxH pxW w h >> flip (++)) [] childs)
            ]

        Leaf p _ kind ->
            let
                w =
                    pxW p w_

                h =
                    pxH p h_
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


css : () -> Html msg
css _ =
    [ text """
.panels {
   float:left;
   position:relative;
}
.content {
    float:left;
    position:relative;
}

""" ]
        |> Html.node "style" []


modalCss : () -> Html msg
modalCss _ =
    [ text """
.modal {
    position:fixed;
    border-radius: 6px;
    z-index:4;
    top:50%;
    left:50%;
    transform: translate(-50%,-50%);
}
.modal-background {
    position:fixed;
    top: 0;
    right: 0;
    bottom: 0;
    left: 0;
    background-color: rgba(0,0,0,0.4);
    z-index:4;
}
""" ]
        |> Html.node "style" []
