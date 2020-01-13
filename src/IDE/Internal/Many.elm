module IDE.Internal.Many exposing
    ( Many
    , init
    , first, second, head, tail, toList, get
    , length, member, tailIsEmpty
    , cons, push, append
    , map, indexedMap, mapFirst, mapSecond, updateAt
    , foldl, foldr, indexedFoldl
    )

{-| A list that have at least two elements. The head and tail can be accessed without Maybes. Most other list functions are
available.


# Definition

@docs Many


# Create

@docs init


# Access

@docs first, second, head, tail, toList, get

head


# Inspect

Nonempty lists support equality with the usual `(==)` operator (provided their contents also support equality).

@docs length, member, tailIsEmpty


# Convert

@docs cons, push, append

pop, reverse, concat


# Swap

docs replaceHead, replaceTail, dropTail


# Map

@docs map, indexedMap, mapFirst, mapSecond, updateAt

map2, andMap, concatMap


# Filter

docs filter


# Fold

To fold or scan from the right, reverse the list first.

@docs foldl, foldr, indexedFoldl


# Zipping

docs zip, unzip


# Sort

docs sort, sortBy, sortWith

-}

import Array exposing (Array)


{-| -}
type alias Many a =
    ( a, a, Array a )


{-| Initialize an Many
-}
init : a -> a -> List a -> Many a
init c1 c2 cs =
    ( c1, c2, Array.fromList cs )


{-| Determine if an element is present in the nonempty list.
-}
member : a -> Many a -> Bool
member y ( c1, c2, cs ) =
    c1 == y || c2 == y || arrayMember y cs


{-| Returns `Just` the element at the given index in the list, or `Nothing` if the index is out of range.
-}
get : Int -> Many a -> Maybe a
get i ( c1, c2, cx ) =
    if i < 0 then
        Nothing

    else
        case i of
            0 ->
                Just c1

            1 ->
                Just c2

            _ ->
                Array.get (i - 2) cx


{-| -}
length : Many a -> Int
length ( c1, c2, cx ) =
    Array.length cx |> (+) 2


{-| Extract the first value.
-}
first : Many a -> a
first ( c1, _, _ ) =
    c1


{-| Extract the second value.
-}
second : Many a -> a
second ( _, c2, _ ) =
    c2


{-| Extract values after two first
-}
tail : Many a -> List a
tail ( _, _, cs ) =
    Array.toList cs


{-| Return the two first elements of the list.
-}
head : Many a -> ( a, a )
head ( c1, c2, _ ) =
    ( c1, c2 )


{-| -}
tailIsEmpty : Many a -> Bool
tailIsEmpty =
    tail >> List.isEmpty


{-| Apply a function on every element.
-}
map : (a -> b) -> Many a -> Many b
map f ( c1, c2, cs ) =
    ( f c1, f c2, Array.map f cs )


{-| Transform the first element.
-}
mapFirst : (a -> a) -> Many a -> Many a
mapFirst f ( c1, c2, cs ) =
    ( f c1, c2, cs )


{-| Transform the second element.
-}
mapSecond : (a -> a) -> Many a -> Many a
mapSecond f ( c1, c2, cs ) =
    ( c1, f c2, cs )


{-| Apply a function on every element with its index as first argument.
-}
indexedMap : (Int -> a -> b) -> Many a -> Many b
indexedMap f ( c1, c2, cs ) =
    ( f 0 c1, f 1 c2, Array.indexedMap (\i -> f (i + 2)) cs )


{-| Reduce an Many from the left. Read foldl as fold from the left.
-}
foldl : (a -> b -> b) -> b -> Many a -> b
foldl f acc ( c1, c2, cs ) =
    Array.foldl f (f c1 acc |> f c2) cs


{-| -}
indexedFoldl : (Int -> a -> b -> b) -> b -> Many a -> b
indexedFoldl f acc ( c1, c2, cs ) =
    arrayIndexedFoldlStart2 f (f 0 c1 acc |> f 1 c2) cs


arrayIndexedFoldlStart2 : (Int -> a -> b -> b) -> b -> Array a -> b
arrayIndexedFoldlStart2 func acc list =
    let
        step : a -> ( Int, b ) -> ( Int, b )
        step x ( i, thisAcc ) =
            ( i + 1, func i x thisAcc )
    in
    Tuple.second (Array.foldl step ( 2, acc ) list)


{-| Reduce an Many from the right. Read foldr as fold from the right.
-}
foldr : (a -> b -> b) -> b -> Many a -> b
foldr f acc ( c1, c2, cs ) =
    Array.foldr f acc cs |> f c1 |> f c2


{-| Append two `Many` to a new one.
-}
append : Many a -> Many a -> Many a
append ( c1, c2, cs ) ( c21, c22, c2s ) =
    ( c1, c2, Array.append (Array.push c21 cs |> Array.push c22) c2s )


{-| push element to end Many.
-}
push : a -> Many a -> Many a
push a ( c1, c2, cs ) =
    ( c1, c2, Array.push a cs )


{-| Add another element as the head of the list, pushing the previous head to the tail.
-}
cons : a -> Many a -> Many a
cons c ( c1, c2, cs ) =
    ( c, c1, Array.foldl Array.push (Array.fromList [ c2 ]) cs )


{-| Create a list of elements.
-}
toList : Many a -> List a
toList ( c1, c2, cs ) =
    c1 :: c2 :: Array.toList cs


{-| Replace a value at a specific index by calling an update function. Return the original list if the index is out of range.
updateAt 0 ((+) 1) [ 1, 2, 3 ]
--> [ 2, 2, 3 ]
See also `updateIfIndex`.
-}
updateAt : Int -> (a -> a) -> Many a -> Many a
updateAt index fn (( c1, c2, cs ) as many) =
    if index < 0 then
        many

    else
        case index of
            0 ->
                mapFirst fn many

            1 ->
                mapSecond fn many

            _ ->
                case Array.get (index - 2) cs of
                    Nothing ->
                        many

                    Just element_ ->
                        ( c1, c2, Array.set (index - 2) (fn element_) cs )


arrayMember : a -> Array a -> Bool
arrayMember x arr =
    arrayAny (\a -> a == x) arr


arrayAny : (a -> Bool) -> Array a -> Bool
arrayAny =
    arrayAny_ 0


arrayAny_ index isOkay arr =
    case Array.get index arr of
        Nothing ->
            False

        Just x ->
            -- note: (isOkay x || any isOkay xs) would not get TCO
            if isOkay x then
                True

            else
                arrayAny_ (index + 1) isOkay arr
