module IDE.UI.Widget.Internal.Math exposing (calc)

import Parser exposing (..)


type Expression
    = Number Float
    | Neg Expression
    | Add Expression Expression
    | Sub Expression Expression
    | Mul Expression Expression
    | Div Expression Expression


expression : Parser Expression
expression =
    lazy (\_ -> chainLeft addOp term)


chainLeft : Parser (Expression -> Expression -> Expression) -> Parser Expression -> Parser Expression
chainLeft op p =
    let
        accumulate x =
            oneOf
                [ op |> andThen (\f -> andThen (f x >> accumulate) p), succeed x ]
    in
    andThen accumulate p


addOp : Parser (Expression -> Expression -> Expression)
addOp =
    oneOf
        [ succeed Add
            |. symbol "+"
        , succeed Sub
            |. symbol "-"
        ]


mulOp : Parser (Expression -> Expression -> Expression)
mulOp =
    oneOf
        [ succeed Mul |. symbol "*"
        , succeed Div |. symbol "/"
        ]


term : Parser Expression
term =
    lazy (\_ -> chainLeft mulOp factor)


factor : Parser Expression
factor =
    succeed identity
        |. spaces
        |= oneOf
            [ succeed Number
                |= Parser.float
            , succeed Neg
                |. symbol "-"
                |= lazy (\_ -> factor)
            , lazy (\_ -> parens expression)
            ]
        |. spaces


parens : Parser a -> Parser a
parens p =
    succeed identity
        |. symbol "("
        |= lazy (\_ -> p)
        |. symbol ")"


parse : Parser Expression
parse =
    succeed identity
        |= expression
        |. end


eval : Expression -> Float
eval expr =
    case expr of
        Number x ->
            x

        Neg x ->
            negate (eval x)

        Add left right ->
            eval left + eval right

        Sub left right ->
            eval left - eval right

        Mul left right ->
            eval left * eval right

        Div left right ->
            eval left / eval right


calc : String -> Result (List DeadEnd) Float
calc expr_ =
    case run parse expr_ of
        Ok expr ->
            Ok (eval expr)

        Err error ->
            Err error
