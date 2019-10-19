module IDE.Internal.ManyAndNotempty exposing (toMany, toNonempty)

import IDE.Internal.Many as Many exposing (Many)
import IDE.Internal.Notempty as Nonempty exposing (Nonempty)


toNonempty : Many a -> Nonempty a
toNonempty many =
    let
        ( a, b ) =
            Many.head many

        rest =
            Many.tail many
                |> Nonempty.init b
    in
    Nonempty.cons a rest


toMany : Nonempty a -> Nonempty a -> Many a
toMany n1 n2 =
    let
        c1 =
            Nonempty.head n1

        c2 =
            Nonempty.head n2

        cs1 =
            Nonempty.tail n1

        cs2 =
            Nonempty.tail n2
    in
    case cs1 of
        x :: xs ->
            Many.init c1 x (c2 :: xs ++ cs2)

        [] ->
            Many.init c1 c2 cs2
