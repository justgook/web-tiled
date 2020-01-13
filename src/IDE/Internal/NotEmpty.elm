module IDE.Internal.NotEmpty exposing
    ( init
    , head, tail, toList, get
    , length, member
    , cons, append, pop, reverse, concat
    , map, indexedMap, map2, andMap, concatMap, updateAt
    , foldl
    , dedup, uniq
    , NotEmpty
    )

{-| A list that cannot be empty. The head and tail can be accessed without Maybes. Most other list functions are
available.


# Definition

@docs Nonempty


# Create

@docs init


# Access

@docs head, tail, toList, get


# Inspect

Nonempty lists support equality with the usual `(==)` operator (provided their contents also support equality).

@docs length, member


# Convert

@docs cons, append, pop, reverse, concat


# Swap

docs replaceHead, replaceTail, dropTail


# Map

@docs map, indexedMap, map2, andMap, concatMap, updateAt


# Filter

docs filter


# Fold

To fold or scan from the right, reverse the list first.

@docs foldl


# Zipping

docs zip, unzip


# Sort

docs sort, sortBy, sortWith


# Deduplicate

The nonempty list's elements must support equality (e.g. not functions). Otherwise you will get a runtime error.

@docs dedup, uniq

-}


{-| The Nonempty type. If you have both a head and tail, you can construct a
nonempty list directly. Otherwise use the helpers below instead.
-}
type alias NotEmpty a =
    ( a, List a )


{-| Create a singleton list with the given element.
-}
init : a -> List a -> NotEmpty a
init a b =
    ( a, b )


{-| Return the head of the list.
-}
head : NotEmpty a -> a
head ( x, xs ) =
    x


{-| Return the tail of the list.
-}
tail : NotEmpty a -> List a
tail ( x, xs ) =
    xs


{-| Reduce a nonempty list from the left with a base case.

    foldl (++) "" (Nonempty "a" [ "b", "c" ]) --> "cba"

-}
foldl : (a -> b -> b) -> b -> NotEmpty a -> b
foldl f b ( x, xs ) =
    List.foldl f b (x :: xs)


{-| Convert to an ordinary list.
-}
toList : NotEmpty a -> List a
toList ( x, xs ) =
    x :: xs


{-| Reverse a nonempty list.
-}
reverse : NotEmpty a -> NotEmpty a
reverse ( x, xs ) =
    let
        revapp : ( List a, a, List a ) -> NotEmpty a
        revapp ( ls, c, rs ) =
            case rs of
                [] ->
                    ( c, ls )

                r :: rss ->
                    revapp ( c :: ls, r, rss )
    in
    revapp ( [], x, xs )


{-| Add another element as the head of the list, pushing the previous head to the tail.
-}
cons : a -> NotEmpty a -> NotEmpty a
cons y ( x, xs ) =
    ( y, x :: xs )


{-| Append two nonempty lists together. `(++)` is _not_ supported.
-}
append : NotEmpty a -> NotEmpty a -> NotEmpty a
append ( x, xs ) ( y, ys ) =
    ( x, xs ++ y :: ys )


{-| Pop and discard the head, or do nothing for a singleton list. Useful if you
want to exhaust a list but hang on to the last item indefinitely.

     pop (Nonempty 3 [ 2, 1 ]) --> Nonempty 2 [1]
     pop (Nonempty 1 []) --> Nonempty 1 []

-}
pop : NotEmpty a -> NotEmpty a
pop ( x, xs ) =
    case xs of
        [] ->
            ( x, xs )

        y :: ys ->
            ( y, ys )


{-| Flatten a nonempty list of nonempty lists into a single nonempty list.
-}
concat : NotEmpty (NotEmpty a) -> NotEmpty a
concat ( xs, xss ) =
    let
        hd =
            head xs

        tl =
            tail xs ++ List.concat (List.map toList xss)
    in
    ( hd, tl )


{-| Remove _adjacent_ duplicate elements from the nonempty list.

     dedup (Nonempty 1 [ 2, 2, 1 ]) --> Nonempty 1 [2, 1]

-}
dedup : NotEmpty a -> NotEmpty a
dedup ( x, xs ) =
    let
        dedupe : a -> List a -> List a -> NotEmpty a
        dedupe prev done next =
            case next of
                [] ->
                    ( prev, done )

                y :: ys ->
                    if y == prev then
                        dedupe prev done ys

                    else
                        dedupe y (prev :: done) ys
    in
    reverse <| dedupe x [] xs


{-| Remove _all_ duplicate elements from the nonempty list, with the remaining elements ordered by first occurrence.

     uniq (Nonempty 1 [ 2, 2, 1 ]) --> Nonempty 1 [2]

-}
uniq : NotEmpty a -> NotEmpty a
uniq ( x, xs ) =
    let
        unique : List a -> NotEmpty a -> List a -> NotEmpty a
        unique seen done next =
            case next of
                [] ->
                    done

                y :: ys ->
                    if List.member y seen then
                        unique seen done ys

                    else
                        unique (y :: seen) (cons y done) ys
    in
    reverse <| unique [ x ] ( x, [] ) xs


{-| Find the length of the nonempty list.
-}
length : NotEmpty a -> Int
length ( _, xs ) =
    List.length xs + 1


{-| Determine if an element is present in the nonempty list.
-}
member : a -> NotEmpty a -> Bool
member y ( x, xs ) =
    x == y || List.member y xs


{-| Returns `Just` the element at the given index in the list, or `Nothing` if the index is out of range.
-}
get : Int -> NotEmpty a -> Maybe a
get i ( x, xs ) =
    if i < 0 then
        Nothing

    else
        case i of
            0 ->
                Just x

            _ ->
                List.head <| List.drop (i - 1) xs


{-| Map a function over a nonempty list.
-}
map : (a -> b) -> NotEmpty a -> NotEmpty b
map f ( x, xs ) =
    ( f x, List.map f xs )


{-| Map a function over two nonempty lists.
-}
map2 : (a -> b -> c) -> NotEmpty a -> NotEmpty b -> NotEmpty c
map2 f ( x, xs ) ( y, ys ) =
    ( f x y, List.map2 f xs ys )


{-| Map over an arbitrary number of nonempty lists.
map2 (,) xs ys == map (,) xs |> andMap ys
head (map (,,) xs |> andMap ys |> andMap zs) == ( head xs, head ys, head zs )
-}
andMap : NotEmpty a -> NotEmpty (a -> b) -> NotEmpty b
andMap =
    map2 (|>)


{-| Map a given function onto a nonempty list and flatten the resulting nonempty lists. If you're chaining, you can
define `andThen = flip concatMap`.
-}
concatMap : (a -> NotEmpty b) -> NotEmpty a -> NotEmpty b
concatMap f xs =
    concat (map f xs)


{-| Same as `map` but the function is also applied to the index of each element (starting at zero).
-}
indexedMap : (Int -> a -> b) -> NotEmpty a -> NotEmpty b
indexedMap f ( x, xs ) =
    let
        wrapped i d =
            f (i + 1) d
    in
    ( f 0 x, List.indexedMap wrapped xs )


{-| Replace a value at a specific index by calling an update function. Return the original list if the index is out of range.
updateAt 0 ((+) 1) [ 1, 2, 3 ]
--> [ 2, 2, 3 ]
See also `updateIfIndex`.
-}
updateAt : Int -> (a -> a) -> NotEmpty a -> NotEmpty a
updateAt index fn ( x, xs ) =
    if index < 0 then
        ( x, xs )

    else if index == 0 then
        ( fn x, xs )

    else
        case List.drop (index - 1) xs of
            x_ :: xs_ ->
                ( x, List.take (index - 1) xs ++ fn x_ :: xs_ )

            _ ->
                ( x, xs )
