module GetLimits exposing (..)

import Expect exposing (Expectation)
import IDE.UI2.Tree as Tree exposing (defaultLimits, setLimits)
import Test exposing (..)


suite : Test
suite =
    describe "UI Limits"
        [ describe "Multilevel Tree"
            [ test "xMax" <|
                \_ ->
                    let
                        a =
                            Tree.node "" |> setLimits { defaultLimits | xMax = Just 100 }

                        b =
                            Tree.node "" |> setLimits { defaultLimits | xMax = Just 200 }

                        c =
                            Tree.fromList
                                ( Tree.node "" |> setLimits { defaultLimits | xMax = Just 200 }
                                , [ Tree.node "" |> setLimits { defaultLimits | xMax = Just 300 } ]
                                )
                    in
                    Tree.fromList ( a, [ b, c ] )
                        |> Tree.getLimitsV
                        |> Expect.equal { defaultLimits | xMax = Just 100 }
            , test "yMax" <|
                \_ ->
                    let
                        a =
                            Tree.node "" |> setLimits { defaultLimits | yMax = Just 100 }

                        b =
                            Tree.node "" |> setLimits { defaultLimits | yMax = Just 200 }

                        c =
                            Tree.fromList
                                ( Tree.node "" |> setLimits { defaultLimits | yMax = Just 200 }
                                , [ Tree.node "" |> setLimits { defaultLimits | yMax = Just 300 } ]
                                )
                    in
                    Tree.fromList ( a, [ b, c ] )
                        |> Tree.getLimitsH
                        |> Expect.equal { defaultLimits | yMax = Just 100 }
            , test "xMax & yMax" <|
                \_ ->
                    let
                        a =
                            Tree.node "" |> setLimits { defaultLimits | xMax = Just 100, yMax = Just 20 }

                        c =
                            Tree.fromList
                                ( Tree.node "" |> setLimits { defaultLimits | xMax = Just 200, yMax = Just 40 }
                                , [ Tree.node "" |> setLimits { defaultLimits | xMax = Just 300, yMax = Just 40 } ]
                                )
                    in
                    Tree.fromList ( a, [ c ] )
                        |> Tree.getLimitsV
                        |> Expect.equal { defaultLimits | xMax = Just 100, yMax = Just 60 }
            , test "xMax,yMax,xMin,yMin" <|
                \_ ->
                    let
                        a =
                            Tree.node ""
                                |> setLimits
                                    { defaultLimits
                                        | xMax = Just 100
                                        , yMax = Just 20
                                        , yMin = 10
                                        , xMin = 100
                                    }

                        c =
                            Tree.fromList
                                ( Tree.node "" |> setLimits { defaultLimits | xMax = Just 200, yMax = Just 40, yMin = 20, xMin = 70 }
                                , [ Tree.node "" |> setLimits { defaultLimits | xMax = Just 300, yMax = Just 40, yMin = 20, xMin = 80 } ]
                                )
                    in
                    Tree.fromList ( a, [ c ] )
                        |> Tree.getLimitsV
                        |> Expect.equal { defaultLimits | xMax = Just 100, yMax = Just 60, yMin = 30, xMin = 150 }
            , test "IDE" <|
                \_ ->
                    let
                        toolbar =
                            Tree.node "toolBar"
                                |> setLimits
                                    { defaultLimits
                                        | xMax = Nothing
                                        , yMax = Just 20
                                        , yMin = 20
                                        , xMin = 100
                                    }

                        content =
                            Tree.fromList
                                ( Tree.node "left" |> setLimits { defaultLimits | xMax = Just 200, yMax = Nothing, yMin = 20, xMin = 100 }
                                , [ Tree.node "center" |> setLimits { defaultLimits | xMax = Nothing, yMax = Nothing, yMin = 300, xMin = 300 }
                                  , Tree.node "right" |> setLimits { defaultLimits | xMax = Just 200, yMax = Nothing, yMin = 20, xMin = 100 }
                                  ]
                                )
                    in
                    Tree.fromList ( toolbar, [ content ] )
                        |> Tree.getLimitsV
                        |> Expect.equal { defaultLimits | xMax = Nothing, yMax = Nothing, yMin = 320, xMin = 500 }
            ]
        , describe "Tree"
            [ describe "vertical block limit"
                [ test "xMax" <|
                    \_ ->
                        let
                            a =
                                Tree.node "" |> setLimits { defaultLimits | xMax = Just 100 }

                            b =
                                Tree.node "" |> setLimits { defaultLimits | xMax = Just 200 }
                        in
                        Tree.fromList ( a, [ b ] )
                            |> Tree.getLimitsV
                            |> Expect.equal { defaultLimits | xMax = Just 100 }
                , test "xMax with unlimited" <|
                    \_ ->
                        let
                            a =
                                Tree.node "" |> setLimits { defaultLimits | xMax = Just 100 }

                            b =
                                Tree.node "" |> setLimits { defaultLimits | xMax = Nothing }
                        in
                        Tree.fromList ( a, [ b ] )
                            |> Tree.getLimitsV
                            |> Expect.equal { defaultLimits | xMax = Just 100 }
                , test "xMin" <|
                    \_ ->
                        let
                            a =
                                Tree.node "" |> setLimits { defaultLimits | xMin = 100 }

                            b =
                                Tree.node "" |> setLimits { defaultLimits | xMin = 230 }
                        in
                        Tree.fromList ( a, [ b ] )
                            |> Tree.getLimitsV
                            |> Expect.equal { defaultLimits | xMin = 230 }
                , test "yMax" <|
                    \_ ->
                        let
                            a =
                                Tree.node "" |> setLimits { defaultLimits | yMax = Just 100 }

                            b =
                                Tree.node "" |> setLimits { defaultLimits | yMax = Just 230 }
                        in
                        Tree.fromList ( a, [ b ] )
                            |> Tree.getLimitsV
                            |> Expect.equal { defaultLimits | yMax = Just 330 }
                ]
            , describe "vertical block limit 3 panels"
                [ test "xMax" <|
                    \_ ->
                        let
                            a =
                                Tree.node "" |> setLimits { defaultLimits | xMax = Just 100 }

                            b =
                                Tree.node "" |> setLimits { defaultLimits | xMax = Just 200 }
                        in
                        Tree.fromList ( a, [ b ] )
                            |> Tree.getLimitsV
                            |> Expect.equal { defaultLimits | xMax = Just 100 }
                ]
            ]
        ]
