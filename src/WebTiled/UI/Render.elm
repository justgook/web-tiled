module WebTiled.UI.Render exposing (Config, view)

import WebTiled.Internal.Many as Many
import WebTiled.UI.Tree exposing (Height, Node(..), Path, Width, horizontalToPlain, mapHorizontalWidth, mapVerticalHeight, verticalToPlain)


type alias Config panel tabInfo html =
    { vResize : Path -> Path -> html
    , hResize : Path -> Path -> html
    , vertical : Width -> List html -> html
    , horizontal : Height -> List html -> html
    , tabsNorth : List ( Path, tabInfo ) -> html
    , tabsEast : List ( Path, tabInfo ) -> html
    , tabsSouth : List ( Path, tabInfo ) -> html
    , tabsWest : List ( Path, tabInfo ) -> html
    , panel : Width -> Height -> panel -> html
    }


view : Path -> Config panel tabInfo html -> Node panel -> html
view path config node_ =
    case node_ of
        VerticalChild w childs_ ->
            let
                ( c1, c2 ) =
                    Many.head childs_

                cs =
                    Many.tail childs_

                view_ i el =
                    el
                        |> verticalToPlain w
                        |> view (path ++ [ i ]) config
            in
            (c2 :: cs)
                |> List.indexedMap
                    (\i el ->
                        [ config.vResize (path ++ [ i ]) (path ++ [ i + 1 ])
                        , view_ (i + 1) el
                        ]
                    )
                |> List.concat
                |> (::) (view_ 0 c1)
                |> config.vertical w

        HorizontalChild h childs_ ->
            let
                ( c1, c2 ) =
                    Many.head childs_

                cs =
                    Many.tail childs_

                view_ i el =
                    el
                        |> horizontalToPlain h
                        |> view (path ++ [ i ]) config
            in
            (c2 :: cs)
                |> List.indexedMap
                    (\i el ->
                        [ config.hResize (path ++ [ i ]) (path ++ [ i + 1 ])
                        , view_ (i + 1) el
                        ]
                    )
                |> List.concat
                |> (::) (view_ 0 c1)
                |> config.horizontal h

        TabbedChild limit w h ( side, focus, ( c1, c2, cs ) ) ->
            Debug.todo "view_::TabbedChild"

        --            let
        --                tabsList =
        --                    indexedFoldr
        --                        (\i cell_ acc ->
        --                            ( Rest i This |> path, config.getTabInfo cell_ ) :: acc
        --                        )
        --                        (( First This |> path, config.getTabInfo c1 ) :: ( Second This |> path, config.getTabInfo c2 ) :: [])
        --                        cs
        --            in
        --            case side of
        --                North ->
        --                    [ config.tabsNorth tabsList
        --                    , config.panel w h c1
        --                    ]
        --                        |> config.vertical w
        --
        --                East ->
        --                    [ config.panel w h c1
        --                    , config.tabsEast tabsList
        --                    ]
        --                        |> config.horizontal h
        --
        --                South ->
        --                    [ config.panel w h c1
        --                    , config.tabsSouth tabsList
        --                    ]
        --                        |> config.vertical w
        --
        --                West ->
        --                    [ config.tabsWest tabsList
        --                    , config.panel w h c1
        --                    ]
        --                        |> config.horizontal h
        SingleChild limit w h panelInfo ->
            config.panel w h panelInfo
