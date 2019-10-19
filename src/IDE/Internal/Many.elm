module IDE.Internal.Many exposing
    ( Many
    , init
    , first, second, head, tail, toList, get
    , length, member, tailIsEmpty
    , cons, push, append
    , map, indexedMap, mapFirst, mapSecond, updateAt
    , foldl, foldr, tailIndexedFoldr
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

@docs foldl, foldr, tailIndexedFoldr


# Zipping

docs zip, unzip


# Sort

docs sort, sortBy, sortWith


# Deduplicate

The nonempty list's elements must support equality (e.g. not functions). Otherwise you will get a runtime error.

dedup, uniq

-}


{-| -}
type alias Many a =
    ( a, a, List a )


{-| Initialize an Many
-}
init : a -> a -> List a -> Many a
init c1 c2 cs =
    ( c1, c2, cs )


{-| Determine if an element is present in the nonempty list.
-}
member : a -> Many a -> Bool
member y ( c1, c2, cs ) =
    c1 == y || c2 == y || List.member y cs


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
                List.head <| List.drop (i - 2) cx


{-| -}
length : Many a -> Int
length =
    tail >> List.length >> (+) 2


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
    cs


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
    ( f c1, f c2, List.map f cs )


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
    ( f 0 c1, f 1 c2, List.indexedMap (\i -> f (i + 2)) cs )


{-| Reduce an Many from the left. Read foldl as fold from the left.
-}
foldl : (a -> b -> b) -> b -> Many a -> b
foldl f acc ( c1, c2, cs ) =
    List.foldl f (f c1 acc |> f c2) cs


{-| -}
tailIndexedFoldr : (Int -> a -> b -> b) -> b -> Many a -> b
tailIndexedFoldr f b =
    tail >> indexedFoldr_ f b


{-| -}
indexedFoldr_ : (Int -> a -> b -> b) -> b -> List a -> b
indexedFoldr_ func acc list =
    let
        step : a -> ( Int, b ) -> ( Int, b )
        step x ( i, thisAcc ) =
            ( i - 1, func i x thisAcc )
    in
    Tuple.second (List.foldr step ( List.length list - 1, acc ) list)


{-| Reduce an Many from the right. Read foldr as fold from the right.
-}
foldr : (a -> b -> b) -> b -> Many a -> b
foldr f acc ( c1, c2, cs ) =
    List.foldr f acc cs |> f c1 |> f c2


{-| Append two `Many` to a new one.
-}
append : Many a -> Many a -> Many a
append ( c1, c2, cs ) a =
    ( c1, c2, cs ++ toList a )


{-| push element to end Many.
-}
push : a -> Many a -> Many a
push a ( c1, c2, cs ) =
    ( c1, c2, cs ++ [ a ] )


{-| Add another element as the head of the list, pushing the previous head to the tail.
-}
cons : a -> Many a -> Many a
cons c ( c1, c2, cs ) =
    ( c, c1, c2 :: cs )


{-| Create a list of elements.
-}
toList : Many a -> List a
toList ( c1, c2, cs ) =
    c1 :: c2 :: cs


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
                case List.drop (index - 2) cs of
                    x :: xs ->
                        ( c1, c2, List.take (index - 2) cs ++ fn x :: xs )

                    _ ->
                        many
