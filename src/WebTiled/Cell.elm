module WebTiled.Cell exposing (Message, Model, Tabbed(..), empty, update, view)

import Element exposing (Element, centerX, centerY, column, el, fill, height, none, px, row, text, width)
import Element.Background as Background
import Html.Attributes as Html
import Html.Events
import Json.Decode as Decode


type alias Many a =
    ( a, a, List a )


type Path
    = First Path
    | Second Path
    | Rest Int Path
    | This


type Child
    = VerticalChild (Maybe Int) (Many VerticalChild)
    | HorizontalChild (Maybe Int) (Many HorizontalChild)
    | TabbedChild (Maybe Int) (Maybe Int) Tabbed
    | SingleChild (Maybe Int) (Maybe Int) Single


type Tabbed
    = Tabbed TabsPosition (Many Single)


type TabsPosition
    = North
    | East
    | South
    | West


type alias Id =
    Int


type HorizontalChild
    = HVerticalChild (Maybe Int) (Many VerticalChild)
    | HTabbedChild (Maybe Int) Tabbed
    | HSingleChild (Maybe Int) Single


type VerticalChild
    = VHorizontalChild (Maybe Int) (Many HorizontalChild)
    | VTabbedChild (Maybe Int) Tabbed
    | VSingleChild (Maybe Int) Single


type alias Single =
    { title : String
    }


type Message
    = ResizeStart ResizeInfo
    | ResizeStop ResizeInfo
    | Resizing ResizeInfo


type alias ResizeInfo =
    { path : Path
    , start :
        { x : Int
        , y : Int
        }
    , diffX : Int
    , diffY : Int
    }


update msg model =
    case msg of
        ResizeStart info ->
            { model | resizing = Just info }

        ResizeStop i ->
            { model | resizing = Nothing }

        Resizing info ->
            let
                resize a =
                    Maybe.map (\aa -> aa - a |> max 0)

                f item =
                    case item of
                        VerticalChild w b ->
                            VerticalChild (resize info.diffX w) b

                        HorizontalChild h b ->
                            HorizontalChild (resize info.diffY h) b

                        TabbedChild w h b ->
                            TabbedChild (resize info.diffX w) (resize info.diffY h) b

                        SingleChild w h b ->
                            SingleChild (resize info.diffX w) (resize info.diffY h) b
            in
            { model
                | layout = setIn f info.path model.layout
                , resizing = Just info
            }


verticalToPlain : VerticalChild -> Child
verticalToPlain child =
    case child of
        VHorizontalChild h b ->
            HorizontalChild h b

        VTabbedChild h b ->
            TabbedChild Nothing h b

        VSingleChild h b ->
            SingleChild Nothing h b


horizontalToPlain : HorizontalChild -> Child
horizontalToPlain child =
    case child of
        HTabbedChild w b ->
            TabbedChild w Nothing b

        HSingleChild w b ->
            SingleChild w Nothing b

        HVerticalChild w b ->
            VerticalChild w b


setIn : (Child -> Child) -> Path -> Child -> Child
setIn f path item =
    case item of
        VerticalChild w ( cell1, cell2, cells ) ->
            case path of
                First p ->
                    case setIn f p (verticalToPlain cell1) of
                        VerticalChild w_ ( c1, c2, cs ) ->
                            VerticalChild w ( c1, c2, cell2 :: cs ++ cells )

                        HorizontalChild h_ i ->
                            VerticalChild w ( VHorizontalChild h_ i, cell2, cells )

                        TabbedChild _ h_ i ->
                            VerticalChild w ( VTabbedChild h_ i, cell2, cells )

                        SingleChild _ h_ i ->
                            VerticalChild w ( VSingleChild h_ i, cell2, cells )

                Second p ->
                    case setIn f p (verticalToPlain cell2) of
                        VerticalChild w_ ( c1, c2, cs ) ->
                            VerticalChild w ( cell1, c1, c2 :: cs ++ cells )

                        HorizontalChild h_ i ->
                            VerticalChild w ( cell1, VHorizontalChild h_ i, cells )

                        TabbedChild _ h_ i ->
                            VerticalChild w ( cell1, VTabbedChild h_ i, cells )

                        SingleChild _ h_ i ->
                            VerticalChild w ( cell1, VSingleChild h_ i, cells )

                Rest index p ->
                    if index < 0 then
                        VerticalChild w ( cell1, cell2, cells )

                    else
                        let
                            head =
                                List.take index cells

                            tail =
                                List.drop index cells
                        in
                        (case tail of
                            x :: xs ->
                                head
                                    ++ (case setIn f p (verticalToPlain x) of
                                            VerticalChild _ ( c1, c2, cs ) ->
                                                c1 :: c2 :: cs

                                            HorizontalChild h_ i_ ->
                                                [ VHorizontalChild h_ i_ ]

                                            TabbedChild _ h_ i_ ->
                                                [ VTabbedChild h_ i_ ]

                                            SingleChild _ h_ i_ ->
                                                [ VSingleChild h_ i_ ]
                                       )
                                    ++ xs

                            _ ->
                                cells
                        )
                            |> (\newCells -> VerticalChild w ( cell1, cell2, newCells ))

                This ->
                    f item

        HorizontalChild h ( cell1, cell2, cells ) ->
            case path of
                First p ->
                    case setIn f p (horizontalToPlain cell1) of
                        VerticalChild w i ->
                            HorizontalChild h ( HVerticalChild w i, cell2, cells )

                        HorizontalChild h_ ( c1, c2, cs ) ->
                            HorizontalChild h ( c1, c2, cell2 :: cs ++ cells )

                        TabbedChild w h_ i ->
                            HorizontalChild h ( HTabbedChild w i, cell2, cells )

                        SingleChild w h_ i ->
                            HorizontalChild h ( HSingleChild w i, cell2, cells )

                Second p ->
                    case setIn f p (horizontalToPlain cell2) of
                        VerticalChild w i ->
                            HorizontalChild h ( cell1, HVerticalChild w i, cells )

                        HorizontalChild h_ ( c1, c2, cs ) ->
                            HorizontalChild h ( cell1, c1, c2 :: cs ++ cells )

                        TabbedChild w h_ i ->
                            HorizontalChild h ( cell1, HTabbedChild w i, cells )

                        SingleChild w h_ i ->
                            HorizontalChild h ( cell1, HSingleChild w i, cells )

                Rest index p ->
                    if index < 0 then
                        HorizontalChild h ( cell1, cell2, cells )

                    else
                        let
                            head =
                                List.take index cells

                            tail =
                                List.drop index cells
                        in
                        (case tail of
                            x :: xs ->
                                head
                                    ++ (case setIn f p (horizontalToPlain x) of
                                            VerticalChild w i_ ->
                                                [ HVerticalChild w i_ ]

                                            HorizontalChild _ ( c1, c2, cs ) ->
                                                c1 :: c2 :: cs

                                            TabbedChild w _ i_ ->
                                                [ HTabbedChild w i_ ]

                                            SingleChild w _ i_ ->
                                                [ HSingleChild w i_ ]
                                       )
                                    ++ xs

                            _ ->
                                cells
                        )
                            |> (\newCells -> HorizontalChild h ( cell1, cell2, newCells ))

                This ->
                    f item

        TabbedChild w h items ->
            Debug.todo "setIn::TabbedChild"

        SingleChild w h items ->
            f item


type alias Model =
    { layout : Child
    , resizing : Maybe ResizeInfo
    }


empty : Model
empty =
    let
        emptyPanel =
            { title = "hello title"
            }

        horInner =
            ( HVerticalChild Nothing vert2
            , HSingleChild (Just 175) { title = "show ME 1" }
            , [ HVerticalChild (Just 200) vert2 ]
            )

        vert1 =
            ( VHorizontalChild Nothing horInner
            , VSingleChild (Just 175) { title = "show ME 2" }
            , [ VSingleChild (Just 200) emptyPanel
              , VSingleChild (Just 200) emptyPanel
              ]
            )

        vert2 =
            ( VSingleChild Nothing emptyPanel
            , VSingleChild (Just 175) { title = "show ME 3" }
            , [ VSingleChild (Just 175) emptyPanel
              , VSingleChild (Just 125) emptyPanel
              ]
            )

        vert3 =
            ( VSingleChild Nothing { title = "Wona This" }
            , VSingleChild (Just 175) { title = "show ME 3" }
            , [ VSingleChild (Just 175) emptyPanel
              , VSingleChild (Just 125) emptyPanel
              ]
            )
    in
    { layout =
        ( HVerticalChild Nothing vert1
        , HSingleChild (Just 175) { title = "show ME 4" }
        , [ HVerticalChild (Just 200) vert3
          , HSingleChild (Just 130) { title = "Or Better" }
          ]
        )
            |> HorizontalChild Nothing
    , resizing = Nothing
    }


type alias LayoutTheme a b =
    { a | panel : { b | background : Element.Color, title : Element.Color } }


view : LayoutTheme a b -> Model -> Element Message
view theme model =
    let
        dragStop info =
            { message = ResizeStop info
            , stopPropagation = True
            , preventDefault = True
            }
                |> Decode.succeed
                |> Html.Events.custom "mouseup"

        ( dragInfo, dragMove ) =
            case model.resizing of
                Just info ->
                    let
                        decoder =
                            Decode.map2
                                (\x y ->
                                    { message =
                                        Resizing
                                            { path = info.path
                                            , diffX = x
                                            , diffY = y
                                            , start = info.start
                                            }
                                    , stopPropagation = True
                                    , preventDefault = True
                                    }
                                )
                                (Decode.field "movementX" Decode.int)
                                (Decode.field "movementY" Decode.int)
                    in
                    decoder
                        |> Html.Events.custom "mousemove"
                        |> Element.htmlAttribute
                        |> (\a attrs ->
                                a
                                    :: Element.htmlAttribute (dragStop info)
                                    :: attrs
                           )
                        |> Tuple.pair (Just { id = info.path, diff = { x = info.diffX, y = info.diffY } })

                _ ->
                    ( Nothing, identity )
    in
    el
        (dragMove [ width fill, height fill ])
        (case model.layout of
            VerticalChild w items ->
                drawVertical theme identity w dragInfo items

            HorizontalChild h items ->
                drawHorizontal theme identity h dragInfo items

            TabbedChild w h items ->
                Debug.todo "render::TabbedChild"

            SingleChild w h items ->
                Debug.todo "render::SingleChild"
        )


drawHorizontal theme path h dragInfo ( cell1, cell2, cells ) =
    let
        rest =
            indexedFoldr
                (\i cell_ acc ->
                    hResize (Rest i This |> path) :: drawHorizontal_ theme (Rest i >> path) dragInfo cell_ :: acc
                )
                []
                cells
    in
    drawHorizontal_ theme (First >> path) dragInfo cell1
        :: hResize (Second This |> path)
        :: drawHorizontal_ theme (Second >> path) dragInfo cell2
        :: rest
        |> row [ height (toLength h), width fill ]


drawHorizontal_ theme path dragInfo cell =
    case cell of
        HVerticalChild w a ->
            drawVertical theme path w dragInfo a

        HTabbedChild w a ->
            drawTabbed theme w Nothing a

        --        HToolBarChild a ->
        --            column [ width fill, height fill ] []
        HSingleChild w a ->
            drawPanel theme.panel w Nothing a


drawVertical theme path w dragId ( cell1, cell2, cells ) =
    let
        rest =
            indexedFoldr
                (\i cell_ acc ->
                    vResize (Rest i This |> path) :: drawVertical_ theme (Rest i >> path) dragId cell_ :: acc
                )
                []
                cells
    in
    drawVertical_ theme (First >> path) dragId cell1
        :: vResize (Second This |> path)
        :: drawVertical_ theme (Second >> path) dragId cell2
        :: rest
        |> column [ width (toLength w), height fill ]


drawVertical_ theme path dragInfo cell =
    case cell of
        VHorizontalChild h a ->
            drawHorizontal theme path h dragInfo a

        VTabbedChild h a ->
            drawTabbed theme Nothing h a

        VSingleChild h a ->
            drawPanel theme.panel Nothing h a



--drawTabbed : LayoutTheme a b -> Tabbed -> Element msg


drawTabbed theme w h (Tabbed pos ( panel1, panel2, panels )) =
    let
        tab { title } =
            el [] <| text <| "tab::" ++ title

        tabs l =
            List.map (\i -> tab i) l
    in
    (\c ->
        case pos of
            North ->
                column [ width fill, height fill ] [ row [ width fill, centerX, centerY ] <| tabs panels, c ]

            East ->
                row [ width fill, height fill ] [ c, column [ height fill, centerX, centerY ] <| tabs panels ]

            South ->
                column [ width fill, height fill ] [ c, row [ width fill, centerX, centerY ] <| tabs panels ]

            West ->
                row [ width fill, height fill ] [ column [ height fill, centerX, centerY ] <| tabs panels, c ]
    )
        (drawPanel theme.panel (Just 200) (Just 200) panel1)


drawPanel theme w h info =
    column
        [ Background.color theme.background
        , width (toLength w)
        , height (toLength h)
        ]
        [ row [ Background.color theme.title, width fill ] [ text info.title ]
        , el [] (text "im panel")
        ]


vResize : Path -> Element Message
vResize path =
    el
        [ height (px 4)
        , Background.color <| Element.rgb255 171 0 0
        , width fill
        , Element.htmlAttribute (Html.style "cursor" "row-resize")
        , Element.htmlAttribute (dragStart path)
        ]
    <|
        none


hResize : Path -> Element Message
hResize path =
    let
        --            E
        extendHover =
            Element.onRight
                (el
                    [ Background.color <| Element.rgb255 0 100 0
                    , width (px 14)
                    , height fill
                    ]
                    none
                )
    in
    el
        [ width (px 4)

        --        , Element.mouseOver [ Element.transparent True ]
        , extendHover
        , Background.color <| Element.rgb255 0 171 0
        , height fill
        , Element.htmlAttribute (Html.style "cursor" "col-resize")
        , Element.htmlAttribute (dragStart path)
        ]
    <|
        none


dragStart path =
    let
        decoder =
            Decode.map2
                (\x y ->
                    { message =
                        ResizeStart
                            { path = path
                            , diffX = x
                            , diffY = y
                            , start =
                                { x = x
                                , y = y
                                }
                            }
                    , stopPropagation = True
                    , preventDefault = True
                    }
                )
                (Decode.field "screenX" Decode.int)
                (Decode.field "screenY" Decode.int)
    in
    Html.Events.custom "mousedown" decoder


toLength : Maybe Int -> Element.Length
toLength =
    Maybe.map px >> Maybe.withDefault fill


indexedFoldr : (Int -> a -> b -> b) -> b -> List a -> b
indexedFoldr func acc list =
    let
        step : a -> ( Int, b ) -> ( Int, b )
        step x ( i, thisAcc ) =
            ( i - 1, func i x thisAcc )
    in
    Tuple.second (List.foldr step ( List.length list - 1, acc ) list)



--
--updateAt : Int -> (a -> a) -> List a -> List a
--updateAt index fn list =
--    if index < 0 then
--        list
--
--    else
--        let
--            head =
--                List.take index list
--
--            tail =
--                List.drop index list
--        in
--        case tail of
--            x :: xs ->
--                head ++ fn x :: xs
--
--            _ ->
--                list
