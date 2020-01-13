module IDE.UI2.Tree exposing
    ( Path
    , Size
    , Tree(..)
    , balance
    , fromList
    , getLimitsH
    , getLimitsV
    , mapAt
    , mapSize
    , node
    , setLimits
    )

import IDE.Internal.Many as Many exposing (Many)
import IDE.Internal.NotEmpty as NotEmpty exposing (NotEmpty)


type alias Limit =
    { xMin : Int
    , xMax : Maybe Int
    , yMin : Int
    , yMax : Maybe Int
    }


type Tree panel
    = Branch Size (Many (Tree panel))
    | Leaf Size Limit panel


mapAt : Path -> (Tree panel -> Tree panel) -> Tree panel -> Tree panel
mapAt path fn tree =
    case path of
        x :: xs ->
            case tree of
                Branch p childs ->
                    Branch p (Many.updateAt x (mapAt xs fn) childs)

                Leaf _ _ _ ->
                    tree

        [] ->
            fn tree


balance : Int -> Int -> Tree panel -> Tree panel
balance w h =
    balance_ ( getLimitsH, .xMax, w ) ( getLimitsV, .yMax, h )


balance_ ( fn11, fn12, w ) ( fn21, fn22, h ) tree =
    let
        limit =
            fn11 tree
    in
    case tree of
        Branch p childs ->
            childs
                |> Many.map
                    (\i ->
                        balance_
                            ( fn21, fn22, fn22 limit |> Maybe.withDefault h )
                            ( fn11, fn12, fn12 limit |> Maybe.withDefault w )
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
                                                if fn21 i |> fn22 |> (/=) Nothing then
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
                                        if fn21 i |> fn22 |> (/=) Nothing then
                                            i

                                        else
                                            mapSize ((*) newSize) i
                                    )
                   )
                |> (fn12 limit
                        |> Maybe.map (\value -> Branch (toFloat value / toFloat w))
                        |> Maybe.withDefault (Branch p)
                   )

        Leaf p l panel ->
            fn12 limit
                |> Maybe.map (\value -> Leaf (toFloat value / toFloat w) l panel)
                |> Maybe.withDefault tree


node : panel -> Tree panel
node panel =
    Leaf 1 defaultLimits panel


setLimits : Limit -> Tree panel -> Tree panel
setLimits limit item =
    case item of
        Leaf p _ panel ->
            Leaf p limit panel

        Branch _ _ ->
            item


getLimitsH tree =
    { xMin = 0, xMax = Just 0, yMin = 0, yMax = Just 0 }
        |> calcLimit verticalLimit horizontalLimit tree


getLimitsV tree =
    { xMin = 0, xMax = Just 0, yMin = 0, yMax = Just 0 }
        |> calcLimit horizontalLimit verticalLimit tree


calcLimit fn1 fn2 tree was =
    case tree of
        Leaf _ limit _ ->
            fn1 limit was

        Branch _ childs ->
            Many.foldl (calcLimit fn1 fn2) { xMin = 0, xMax = Just 0, yMin = 0, yMax = Just 0 } childs
                |> fn2 was


verticalLimit a b =
    { xMin = max a.xMin b.xMin
    , xMax = Maybe.map2 max a.xMax b.xMax
    , yMin = a.yMin + b.yMin
    , yMax = Maybe.map2 (+) a.yMax b.yMax
    }


horizontalLimit a b =
    { xMin = a.xMin + b.xMin
    , xMax = Maybe.map2 (+) a.xMax b.xMax
    , yMin = max a.yMin b.yMin
    , yMax = Maybe.map2 max a.yMax b.yMax
    }


defaultLimits =
    { xMin = 10
    , xMax = Nothing
    , yMin = 10
    , yMax = Nothing
    }


fromList : NotEmpty (Tree panel) -> Tree panel
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
                |> Branch 1


mapSize : (Size -> Size) -> Tree panel -> Tree panel
mapSize fn item =
    case item of
        Branch p childs ->
            Branch (fn p) childs

        Leaf p limit1 panel ->
            Leaf (fn p) limit1 panel


size : Tree panel -> Size
size item =
    case item of
        Branch p childs ->
            p

        Leaf p limit1 panel ->
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


applyIf : Bool -> (a -> a) -> a -> a
applyIf bool f world =
    if bool then
        f world

    else
        world
