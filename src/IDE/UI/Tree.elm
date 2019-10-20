module IDE.UI.Tree exposing
    ( Height
    , Node(..)
    , Path
    , Width
    , addEast
    , addNorth
    , addSouth
    , addWest
    , horizontalToPlain
    , mapAt
    , mapHorizontalWidth
    , mapSizeAt
    , mapVerticalHeight
    , node
    , nodeWith
    , setSize
    , verticalToPlain
    )

import IDE.Internal.Many as Many exposing (Many)
import IDE.Internal.ManyAndNotempty exposing (toMany, toNonempty)
import IDE.Internal.Notempty as Notempty


type alias Width =
    Int


type alias Height =
    Int


type alias Path =
    List Int


type Side
    = North
    | East
    | South
    | West


type HorizontalChild panel
    = HVerticalChild Width (Many (VerticalChild panel))
    | HTabbedChild Limit Width (Tabbed panel)
    | HSingleChild Limit Width panel


type VerticalChild panel
    = VHorizontalChild Height (Many (HorizontalChild panel))
    | VTabbedChild Limit Height (Tabbed panel)
    | VSingleChild Limit Height panel


type alias Tabbed panel =
    ( Side, Path, Many panel )


type Node panel
    = VerticalChild Width (Many (VerticalChild panel))
    | HorizontalChild Height (Many (HorizontalChild panel))
    | TabbedChild Limit Width Height (Tabbed panel)
    | SingleChild Limit Width Height panel


type alias Limit =
    { minHeight : Height
    , maxHeight : Maybe Height
    , minWidth : Width
    , maxWidth : Maybe Width
    }


defaultLimit : Limit
defaultLimit =
    { minHeight = 0
    , maxHeight = Nothing
    , minWidth = 0
    , maxWidth = Nothing
    }


node : a -> Node a
node panel =
    SingleChild defaultLimit 2 2 panel


nodeWith : Width -> Height -> a -> Node a
nodeWith w h panel =
    SingleChild defaultLimit w h panel


addEast : Node panel -> Node panel -> Node panel
addEast item to =
    case to of
        VerticalChild w childs ->
            VerticalChild w childs
                |> Debug.todo "addEast::VerticalChild"

        HorizontalChild h childs ->
            let
                ( endW, endH ) =
                    getSize to
            in
            case item of
                SingleChild limit w2 h2 panel2 ->
                    Many.push (HSingleChild limit w2 panel2) childs
                        |> HorizontalChild h
                        |> setSize endW endH

                HorizontalChild _ childs2 ->
                    Many.append childs childs2
                        |> HorizontalChild h
                        |> setSize endW endH

                TabbedChild limit w2 g2 tabbed ->
                    Debug.todo "addEast::TabbedChild"

                VerticalChild w2 childs2 ->
                    Many.push (HVerticalChild w2 childs2) childs
                        |> HorizontalChild h
                        |> setSize endW endH

        TabbedChild limit w h tabbed ->
            TabbedChild limit w h tabbed
                |> Debug.todo "addEast::TabbedChild"

        SingleChild limit1 w1 h1 panel1 ->
            let
                w1_ =
                    max 1 (w1 // 2)

                w2_ =
                    max 1 (w1 - w1_)
            in
            case item of
                SingleChild limit2 _ _ panel2 ->
                    Many.init (HSingleChild limit1 w1_ panel1) (HSingleChild limit2 w2_ panel2) []
                        |> HorizontalChild h1

                _ ->
                    Debug.todo "addEast::SingleChild"


addNorth : Node panel -> Node panel -> Node panel
addNorth item to =
    case to of
        VerticalChild w childs ->
            Debug.todo "addNorth::VerticalChild"

        HorizontalChild _ childs1 ->
            let
                ( w1, h1 ) =
                    getSize to

                --
                --                _ =
                --                    Many.cons (VHorizontalChild h childs)
            in
            case item of
                SingleChild limit w2 h2 panel ->
                    Debug.todo "addNorth::HorizontalChild -> SingleChild"

                HorizontalChild h2 childs2 ->
                    Many.init (VHorizontalChild h1 childs2) (VHorizontalChild h1 childs1) []
                        |> VerticalChild w1

                _ ->
                    Debug.todo "addNorth::HorizontalChild -> Rest"

        TabbedChild limit w h tabbed ->
            Debug.todo "addNorth::TabbedChild"

        SingleChild limit1 w1 h1 panel1 ->
            case item of
                SingleChild limit2 w2 h2 panel2 ->
                    let
                        h1_ =
                            max 1 (h1 // 2)

                        h2_ =
                            max 1 (h1 - h1_)
                    in
                    VerticalChild w1 (Many.init (VSingleChild limit2 h2_ panel2) (VSingleChild limit1 h1_ panel1) [])

                _ ->
                    Debug.todo "addNorth::SingleChild:Rest"


addSouth : Node panel -> Node panel -> Node panel
addSouth item to =
    case to of
        VerticalChild w childs ->
            Debug.todo "addSouth::VerticalChild"

        HorizontalChild _ childs ->
            let
                ( w, h ) =
                    getSize to

                _ =
                    Many.cons (VHorizontalChild h childs)
            in
            --            VerticalChild
            Debug.todo "addSouth::HorizontalChild"

        TabbedChild limit w h tabbed ->
            Debug.todo "addSouth::TabbedChild"

        SingleChild limit1 w1 h1 panel1 ->
            case item of
                SingleChild limit2 w2 h2 panel2 ->
                    let
                        h1_ =
                            max 1 (h1 // 2)

                        h2_ =
                            max 1 (h1 - h1_)
                    in
                    VerticalChild w1 (Many.init (VSingleChild limit1 h1_ panel1) (VSingleChild limit2 h2_ panel2) [])

                _ ->
                    Debug.todo "addSouth::SingleChild:Rest"


addWest : Node panel -> Node panel -> Node panel
addWest item to =
    Debug.todo "addWest"


mapSizeAt : Path -> (Width -> Width) -> (Height -> Height) -> Node panel -> Node panel
mapSizeAt path fW fH node1 =
    mapAt path (mapSize fW fH) node1


mapAt : Path -> (Node panel -> Node panel) -> Node panel -> Node panel
mapAt path f node1 =
    case node1 of
        VerticalChild w childs ->
            case path of
                [] ->
                    f node1

                x :: xs ->
                    Many.updateAt x
                        (\a ->
                            case verticalToPlain w a |> mapAt xs f of
                                VerticalChild _ _ ->
                                    a

                                TabbedChild limit _ h_ tabbed ->
                                    VTabbedChild limit h_ tabbed

                                SingleChild limit _ h_ panel ->
                                    VSingleChild limit h_ panel

                                HorizontalChild h_ panel ->
                                    VHorizontalChild h_ panel
                        )
                        childs
                        |> VerticalChild w

        HorizontalChild h childs ->
            case path of
                [] ->
                    f node1

                x :: xs ->
                    Many.updateAt x
                        (\a ->
                            case horizontalToPlain h a |> mapAt xs f of
                                VerticalChild w_ aaa ->
                                    HVerticalChild w_ aaa

                                TabbedChild limit w_ _ tabbed ->
                                    HTabbedChild limit w_ tabbed

                                SingleChild limit w_ _ panel ->
                                    HSingleChild limit w_ panel

                                HorizontalChild _ _ ->
                                    a
                        )
                        childs
                        |> HorizontalChild h

        TabbedChild limit w h ( side, focus, panels ) ->
            case path of
                [] ->
                    f node1

                _ ->
                    Debug.todo "Tree::updateAt::TabbedChild"

        SingleChild limit w h panel ->
            case path of
                [] ->
                    f node1

                _ ->
                    node1


setSize : Width -> Height -> Node a -> Node a
setSize width height =
    mapSize (\_ -> width) (\_ -> height)


mapSize : (Width -> Width) -> (Height -> Height) -> Node panel -> Node panel
mapSize fW fH node1 =
    case node1 of
        VerticalChild _ childs ->
            let
                ( w, h ) =
                    getSize node1

                incomeW =
                    fW w

                incomeH =
                    fH h

                ratioW =
                    toFloat incomeW / toFloat w

                ratioH =
                    toFloat incomeH / toFloat h

                newChilds =
                    childs
                        |> Many.map (\i -> updateSizeVertical ratioW ratioH i)

                newNode =
                    VerticalChild incomeW newChilds

                ( _, newH ) =
                    getSize newNode

                diffH =
                    incomeH - newH
            in
            if diffH > 0 then
                newChilds
                    |> Many.mapFirst (mapVerticalHeight ((+) diffH))
                    |> VerticalChild incomeW

            else
                newNode

        HorizontalChild _ childs ->
            let
                ( w, h ) =
                    getSize node1

                incomeW =
                    fW w

                incomeH =
                    fH h

                ratioW =
                    toFloat incomeW / toFloat w

                ratioH =
                    toFloat incomeH / toFloat h

                newChilds =
                    childs
                        |> Many.map (\i -> updateSizeHorizontal ratioW ratioH i)

                newNode =
                    HorizontalChild incomeH newChilds

                ( newW, _ ) =
                    getSize newNode

                diffW =
                    incomeW - newW
            in
            if diffW > 0 then
                newChilds
                    |> Many.mapFirst (mapHorizontalWidth ((+) diffW))
                    |> HorizontalChild incomeH

            else
                newNode

        TabbedChild limit w h tabbed ->
            TabbedChild limit (fW w) (fH h) tabbed

        SingleChild limit w h panel ->
            SingleChild limit (fW w) (fH h) panel


updateSizeHorizontal : Float -> Float -> HorizontalChild panel -> HorizontalChild panel
updateSizeHorizontal ratioW ratioH node1 =
    let
        newWidth =
            toFloat >> (*) ratioW >> floor >> max 1
    in
    case node1 of
        HVerticalChild w childs ->
            childs
                |> Many.map (updateSizeVertical ratioW ratioH)
                |> HVerticalChild (newWidth w)

        HTabbedChild limit w tabbed ->
            HTabbedChild limit (newWidth w) tabbed

        HSingleChild limit w panel ->
            HSingleChild limit (newWidth w) panel


updateSizeVertical : Float -> Float -> VerticalChild panel -> VerticalChild panel
updateSizeVertical ratioW ratioH node1 =
    let
        newHeight =
            toFloat >> (*) ratioH >> floor >> max 1
    in
    case node1 of
        VHorizontalChild h childs ->
            childs
                |> Many.map (updateSizeHorizontal ratioW ratioH)
                |> VHorizontalChild (newHeight h)

        VTabbedChild limit w tabbed ->
            VTabbedChild limit (newHeight w) tabbed

        VSingleChild limit w panel ->
            VSingleChild limit (newHeight w) panel


verticalToPlain : Width -> VerticalChild panel -> Node panel
verticalToPlain w child =
    case child of
        VHorizontalChild h b ->
            HorizontalChild h b

        VTabbedChild limit h b ->
            TabbedChild limit w h b

        VSingleChild limit h b ->
            SingleChild limit w h b


mapVerticalHeight : (Height -> Height) -> VerticalChild panel -> VerticalChild panel
mapVerticalHeight f node1 =
    case node1 of
        VTabbedChild limit h rest ->
            VTabbedChild limit (f h) rest

        VSingleChild limit h rest ->
            VSingleChild limit (f h) rest

        VHorizontalChild h rest ->
            VHorizontalChild (f h) rest


horizontalToPlain : Height -> HorizontalChild panel -> Node panel
horizontalToPlain h child =
    case child of
        HTabbedChild limit w b ->
            TabbedChild limit w h b

        HSingleChild limit w b ->
            SingleChild limit w h b

        HVerticalChild w b ->
            VerticalChild w b


mapHorizontalWidth : (Width -> Width) -> HorizontalChild panel -> HorizontalChild panel
mapHorizontalWidth f node1 =
    case node1 of
        HVerticalChild w rest ->
            HVerticalChild (f w) rest

        HTabbedChild limit w rest ->
            HTabbedChild limit (f w) rest

        HSingleChild limit w rest ->
            HSingleChild limit (f w) rest


getSize : Node panel -> ( Width, Height )
getSize node1 =
    let
        getHeight_ : VerticalChild panel -> Height
        getHeight_ a =
            case a of
                VHorizontalChild h _ ->
                    h

                VTabbedChild _ h _ ->
                    h

                VSingleChild _ h _ ->
                    h

        getWidth_ : HorizontalChild panel -> Width
        getWidth_ a =
            case a of
                HVerticalChild w _ ->
                    w

                HTabbedChild _ w _ ->
                    w

                HSingleChild _ w _ ->
                    w
    in
    case node1 of
        VerticalChild w childs ->
            ( w, Many.foldl (getHeight_ >> (+)) 0 childs )

        HorizontalChild h childs ->
            ( Many.foldl (getWidth_ >> (+)) 0 childs, h )

        SingleChild _ w h _ ->
            ( w, h )

        TabbedChild _ w h _ ->
            ( w, h )
