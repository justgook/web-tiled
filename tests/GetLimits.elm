module GetLimits exposing (..)

import Expect exposing (Expectation)
import IDE.UI.Layout as Layout exposing (defaultLimits, setLimits)
import Test exposing (..)


suite : Test
suite =
    describe "UI Limits"
        [ describe "Multilevel Tree"
            [ test "xMax" <|
                \_ ->
                    let
                        a =
                            Layout.node "" |> setLimits { defaultLimits | xMax = Just 100 }

                        b =
                            Layout.node "" |> setLimits { defaultLimits | xMax = Just 200 }

                        c =
                            Layout.fromList
                                ( Layout.node "" |> setLimits { defaultLimits | xMax = Just 200 }
                                , [ Layout.node "" |> setLimits { defaultLimits | xMax = Just 300 } ]
                                )
                    in
                    Layout.fromList ( a, [ b, c ] )
                        |> Layout.getLimitsV
                        |> Expect.equal { defaultLimits | xMax = Just 100 }
            , test "yMax" <|
                \_ ->
                    let
                        a =
                            Layout.node "" |> setLimits { defaultLimits | yMax = Just 100 }

                        b =
                            Layout.node "" |> setLimits { defaultLimits | yMax = Just 200 }

                        c =
                            Layout.fromList
                                ( Layout.node "" |> setLimits { defaultLimits | yMax = Just 200 }
                                , [ Layout.node "" |> setLimits { defaultLimits | yMax = Just 300 } ]
                                )
                    in
                    Layout.fromList ( a, [ b, c ] )
                        |> Layout.getLimitsH
                        |> Expect.equal { defaultLimits | yMax = Just 100 }
            , test "xMax & yMax" <|
                \_ ->
                    let
                        a =
                            Layout.node "" |> setLimits { defaultLimits | xMax = Just 100, yMax = Just 20 }

                        c =
                            Layout.fromList
                                ( Layout.node "" |> setLimits { defaultLimits | xMax = Just 200, yMax = Just 40 }
                                , [ Layout.node "" |> setLimits { defaultLimits | xMax = Just 300, yMax = Just 40 } ]
                                )
                    in
                    Layout.fromList ( a, [ c ] )
                        |> Layout.getLimitsV
                        |> Expect.equal { defaultLimits | xMax = Just 100, yMax = Just 60 }
            , test "xMax,yMax,xMin,yMin" <|
                \_ ->
                    let
                        a =
                            Layout.node ""
                                |> setLimits
                                    { defaultLimits
                                        | xMax = Just 100
                                        , yMax = Just 20
                                        , yMin = 10
                                        , xMin = 100
                                    }

                        c =
                            Layout.fromList
                                ( Layout.node "" |> setLimits { defaultLimits | xMax = Just 200, yMax = Just 40, yMin = 20, xMin = 70 }
                                , [ Layout.node "" |> setLimits { defaultLimits | xMax = Just 300, yMax = Just 40, yMin = 20, xMin = 80 } ]
                                )
                    in
                    Layout.fromList ( a, [ c ] )
                        |> Layout.getLimitsV
                        |> Expect.equal { defaultLimits | xMax = Just 100, yMax = Just 60, yMin = 30, xMin = 150 }
            , test "IDE" <|
                \_ ->
                    let
                        toolbar =
                            Layout.node "toolBar"
                                |> setLimits
                                    { defaultLimits
                                        | xMax = Nothing
                                        , yMax = Just 20
                                        , yMin = 20
                                        , xMin = 100
                                    }

                        content =
                            Layout.fromList
                                ( Layout.node "left" |> setLimits { defaultLimits | xMax = Just 200, yMax = Nothing, yMin = 20, xMin = 100 }
                                , [ Layout.node "center" |> setLimits { defaultLimits | xMax = Nothing, yMax = Nothing, yMin = 300, xMin = 300 }
                                  , Layout.node "right" |> setLimits { defaultLimits | xMax = Just 200, yMax = Nothing, yMin = 20, xMin = 100 }
                                  ]
                                )
                    in
                    Layout.fromList ( toolbar, [ content ] )
                        |> Layout.getLimitsV
                        |> Expect.equal { defaultLimits | xMax = Nothing, yMax = Nothing, yMin = 320, xMin = 500 }
            ]
        , describe "Tree"
            [ describe "vertical block limit"
                [ test "xMax" <|
                    \_ ->
                        let
                            a =
                                Layout.node "" |> setLimits { defaultLimits | xMax = Just 100 }

                            b =
                                Layout.node "" |> setLimits { defaultLimits | xMax = Just 200 }
                        in
                        Layout.fromList ( a, [ b ] )
                            |> Layout.getLimitsV
                            |> Expect.equal { defaultLimits | xMax = Just 100 }
                , test "xMax with unlimited" <|
                    \_ ->
                        let
                            a =
                                Layout.node "" |> setLimits { defaultLimits | xMax = Just 100 }

                            b =
                                Layout.node "" |> setLimits { defaultLimits | xMax = Nothing }
                        in
                        Layout.fromList ( a, [ b ] )
                            |> Layout.getLimitsV
                            |> Expect.equal { defaultLimits | xMax = Just 100 }
                , test "xMin" <|
                    \_ ->
                        let
                            a =
                                Layout.node "" |> setLimits { defaultLimits | xMin = 100 }

                            b =
                                Layout.node "" |> setLimits { defaultLimits | xMin = 230 }
                        in
                        Layout.fromList ( a, [ b ] )
                            |> Layout.getLimitsV
                            |> Expect.equal { defaultLimits | xMin = 230 }
                , test "yMax" <|
                    \_ ->
                        let
                            a =
                                Layout.node "" |> setLimits { defaultLimits | yMax = Just 100 }

                            b =
                                Layout.node "" |> setLimits { defaultLimits | yMax = Just 230 }
                        in
                        Layout.fromList ( a, [ b ] )
                            |> Layout.getLimitsV
                            |> Expect.equal { defaultLimits | yMax = Just 330 }
                ]
            , describe "vertical block limit 3 panels"
                [ test "xMax" <|
                    \_ ->
                        let
                            a =
                                Layout.node "" |> setLimits { defaultLimits | xMax = Just 100 }

                            b =
                                Layout.node "" |> setLimits { defaultLimits | xMax = Just 200 }
                        in
                        Layout.fromList ( a, [ b ] )
                            |> Layout.getLimitsV
                            |> Expect.equal { defaultLimits | xMax = Just 100 }
                ]
            ]
        ]
