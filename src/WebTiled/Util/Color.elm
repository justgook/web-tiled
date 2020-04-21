module WebTiled.Util.Color exposing (fromHex)


colorClamp : Float -> Int
colorClamp n =
    clamp 0 255 (round n)


fromHex : String -> Maybe { r : Float, g : Float, b : Float }
fromHex str =
    let
        withoutHash =
            if String.startsWith "#" str then
                String.dropLeft 1 str

            else
                str
    in
    case String.toList withoutHash of
        [ r1, r2, g1, g2, b1, b2 ] ->
            maybeMap6
                (\a b c d e f ->
                    { r = (a * 16 + b) / 255, g = (c * 16 + d) / 255, b = (e * 16 + f) / 255 }
                )
                (intFromHexChar r1)
                (intFromHexChar r2)
                (intFromHexChar g1)
                (intFromHexChar g2)
                (intFromHexChar b1)
                (intFromHexChar b2)

        _ ->
            Nothing


intFromHexChar : Char -> Maybe Float
intFromHexChar s =
    case s of
        '0' ->
            Just 0

        '1' ->
            Just 1

        '2' ->
            Just 2

        '3' ->
            Just 3

        '4' ->
            Just 4

        '5' ->
            Just 5

        '6' ->
            Just 6

        '7' ->
            Just 7

        '8' ->
            Just 8

        '9' ->
            Just 9

        'a' ->
            Just 10

        'b' ->
            Just 11

        'c' ->
            Just 12

        'd' ->
            Just 13

        'e' ->
            Just 14

        'f' ->
            Just 15

        _ ->
            Nothing


maybeMap6 : (a -> b -> c -> d -> e -> f -> value) -> Maybe a -> Maybe b -> Maybe c -> Maybe d -> Maybe e -> Maybe f -> Maybe value
maybeMap6 func ma mb mc md me mf =
    case ma of
        Nothing ->
            Nothing

        Just a ->
            case mb of
                Nothing ->
                    Nothing

                Just b ->
                    case mc of
                        Nothing ->
                            Nothing

                        Just c ->
                            case md of
                                Nothing ->
                                    Nothing

                                Just d ->
                                    case me of
                                        Nothing ->
                                            Nothing

                                        Just e ->
                                            case mf of
                                                Nothing ->
                                                    Nothing

                                                Just f ->
                                                    Just (func a b c d e f)
