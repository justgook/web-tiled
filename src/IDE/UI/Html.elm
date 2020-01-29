module IDE.UI.Html exposing (modal, view)

import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Lazy
import IDE.Internal.Many as Many exposing (Many)
import IDE.UI.Layout as Layout exposing (Layout(..))


view : (Int -> Int -> Int -> Int -> node -> a) -> Int -> Int -> Layout node -> List a
view fn w h m =
    wrapper fn w h m


modal : (Int -> Int -> Int -> Int -> node -> Html msg) -> Int -> Int -> Layout node -> List (Html msg)
modal fn w_ h_ m =
    let
        { xMax, yMax, xMin, yMin } =
            Layout.getLimitsV m

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


wrapper : (Int -> Int -> Int -> Int -> node -> a) -> Int -> Int -> Layout node -> List a
wrapper fn w h m =
    let
        pxW p a =
            a |> toFloat |> (*) p |> floor

        pxH _ a =
            a
    in
    wrapper_ fn pxW pxH w h 0 0 m


wrapper_ : (number -> number -> number -> number -> node -> a) -> (Layout.Size -> number -> number) -> (Layout.Size -> number -> number) -> number -> number -> number -> number -> Layout node -> List a
wrapper_ fn applyW applyH w_ h_ x y tree =
    case tree of
        Layout p childs ->
            let
                w =
                    applyW p w_

                h =
                    applyH p h_
            in
            Many.foldl (wrapperFold fn applyH applyW w h (h == h_)) ( [], ( x, y ) ) childs
                |> Tuple.first

        Node p _ kind ->
            let
                w =
                    applyW p w_

                h =
                    applyH p h_
            in
            [ fn w h x y kind ]


wrapperFold :
    (number -> number -> number -> number -> node -> a)
    -> (Layout.Size -> number -> number)
    -> (Layout.Size -> number -> number)
    -> number
    -> number
    -> Bool
    -> Layout node
    -> ( List a, ( number, number ) )
    -> ( List a, ( number, number ) )
wrapperFold fn applyW applyH w h isVertical tree ( acc, ( x2, y2 ) ) =
    let
        p =
            Layout.size tree

        xy =
            if isVertical then
                ( x2, y2 + applyH p h )

            else
                ( x2 + applyW p w, y2 )
    in
    ( acc ++ wrapper_ fn applyW applyH w h x2 y2 tree, xy )


px : Int -> String
px i =
    String.fromInt i ++ "px"


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
