module IDE.UI.Layout exposing
    ( Layout(..)
    , Path
    , Size
    , balance
    , defaultLimits
    , fromList
    , getLimitsH
    , getLimitsV
    , mapAt
    , mapSize
    , node
    , setLimits
    , size
    )

import IDE.Internal.Many as Many exposing (Many)
import IDE.Internal.NotEmpty as NotEmpty exposing (NotEmpty)


type alias Limit =
    { xMin : Int
    , xMax : Maybe Int
    , yMin : Int
    , yMax : Maybe Int
    }


type Layout node
    = Layout Size (Many (Layout node))
    | Node Size Limit node


mapAt : Path -> (Layout panel -> Layout panel) -> Layout panel -> Layout panel
mapAt path fn tree =
    case path of
        x :: xs ->
            case tree of
                Layout p childs ->
                    Layout p (Many.updateAt x (mapAt xs fn) childs)

                Node _ _ _ ->
                    tree

        [] ->
            fn tree


balance : Int -> Int -> Layout panel -> Layout panel
balance w h =
    balance_
        { getLimit = getLimitsV
        , getMax = .xMax
        , getMix = .xMin
        , value = w
        }
        { getLimit = getLimitsH
        , getMax = .yMax
        , getMix = .yMin
        , value = h
        }


balance_ a b tree =
    let
        limit =
            a.getLimit tree
    in
    case tree of
        Layout p childs ->
            childs
                |> Many.map
                    (\i ->
                        balance_
                            { b | value = b.getMax limit |> Maybe.withDefault b.value }
                            { a | value = a.getMax limit |> Maybe.withDefault a.value }
                            i
                    )
                |> (\newChilds ->
                        if newChilds == childs then
                            newChilds

                        else
                            let
                                ( left, forChilds ) =
                                    newChilds
                                        |> Many.foldl
                                            (\i ( taken, count ) ->
                                                if b.getLimit i |> b.getMax |> (/=) Nothing then
                                                    ( taken - size i, count )

                                                else
                                                    ( taken, size i + count )
                                            )
                                            ( 1, 0 )

                                newSize =
                                    left / forChilds
                            in
                            newChilds
                                |> Many.map
                                    (\i ->
                                        if b.getLimit i |> b.getMax |> (/=) Nothing then
                                            i

                                        else
                                            mapSize ((*) newSize) i
                                    )
                   )
                |> (a.getMax limit
                        |> Maybe.map (\value -> Layout (toFloat value / toFloat a.value))
                        |> Maybe.withDefault (Layout p)
                   )

        Node _ l panel ->
            a.getMax limit
                |> Maybe.map (\value -> Node (toFloat value / toFloat a.value) l panel)
                |> Maybe.withDefault tree


node : panel -> Layout panel
node panel =
    Node 1 defaultLimits panel


setLimits : Limit -> Layout panel -> Layout panel
setLimits limit item =
    case item of
        Node p _ panel ->
            Node p limit panel

        Layout _ _ ->
            item


getLimitsH tree =
    { xMin = 0, xMax = Nothing, yMin = 0, yMax = Just 0 }
        |> calcLimit
            verticalLimit
            horizontalLimit
            { xMin = 0, xMax = Nothing, yMin = 0, yMax = Just 0 }
            { xMin = 0, xMax = Just 0, yMin = 0, yMax = Nothing }
            tree


getLimitsV tree =
    { xMin = 0, xMax = Just 0, yMin = 0, yMax = Nothing }
        |> calcLimit
            horizontalLimit
            verticalLimit
            { xMin = 0, xMax = Just 0, yMin = 0, yMax = Nothing }
            { xMin = 0, xMax = Nothing, yMin = 0, yMax = Just 0 }
            tree


calcLimit fn1 fn2 init1 init2 tree was =
    case tree of
        Node _ limit _ ->
            fn1 was limit

        Layout _ childs ->
            Many.foldl (calcLimit fn2 fn1 init2 init1) init2 childs
                |> fn1 was


verticalLimit a b =
    { xMin = max a.xMin b.xMin
    , xMax = maybeDo2 min a.xMax b.xMax
    , yMin = a.yMin + b.yMin
    , yMax = Maybe.map2 (+) a.yMax b.yMax
    }


horizontalLimit a b =
    { xMin = a.xMin + b.xMin
    , xMax = Maybe.map2 (+) a.xMax b.xMax
    , yMin = max a.yMin b.yMin
    , yMax = maybeDo2 min a.yMax b.yMax
    }


defaultLimits =
    { xMin = 0, xMax = Nothing, yMin = 0, yMax = Nothing }


fromList : NotEmpty (Layout panel) -> Layout panel
fromList list =
    case list of
        ( item, [] ) ->
            item

        ( item1, item2 :: rest ) ->
            let
                fLength =
                    toFloat (NotEmpty.length list)

                fn =
                    (*) (1 / fLength)
            in
            Many.init item1 item2 rest
                |> Many.map (mapSize fn)
                |> Layout 1


mapSize : (Size -> Size) -> Layout panel -> Layout panel
mapSize fn item =
    case item of
        Layout p childs ->
            Layout (fn p) childs

        Node p limit1 panel ->
            Node (fn p) limit1 panel


size : Layout panel -> Size
size item =
    case item of
        Layout p _ ->
            p

        Node p _ _ ->
            p


type alias Size =
    Float


type alias Path =
    List Int


maybeDo2 : (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
maybeDo2 fn a b =
    case ( a, b ) of
        ( Just a_, Just b_ ) ->
            Just (fn a_ b_)

        ( Just a_, Nothing ) ->
            Just a_

        ( Nothing, Just b_ ) ->
            Just b_

        ( Nothing, Nothing ) ->
            Nothing
