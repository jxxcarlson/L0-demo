module Parser.Symbol exposing (Symbol(..), balance, convertTokens, convertTokens2, toString, value)

import Maybe.Extra
import Parser.Token exposing (Token(..))


type Symbol
    = L
    | R
    | O
    | M
    | C


value : Symbol -> Int
value symbol =
    case symbol of
        L ->
            1

        R ->
            -1

        O ->
            0

        M ->
            0

        C ->
            0


balance : List Symbol -> Int
balance symbols =
    symbols |> List.map value |> List.sum


symbolToString : Symbol -> String
symbolToString symbol =
    case symbol of
        L ->
            "L"

        R ->
            "R"

        O ->
            "O"

        M ->
            "M"

        C ->
            "C"


toString : List Symbol -> String
toString symbols =
    List.map symbolToString symbols |> String.join " "


convertTokens : List Token -> List Symbol
convertTokens tokens =
    List.map toSymbol tokens |> Maybe.Extra.values


convertTokens2 : List Token -> List Symbol
convertTokens2 tokens =
    List.map toSymbol2 tokens


toSymbol : Token -> Maybe Symbol
toSymbol token =
    case token of
        LB _ ->
            Just L

        RB _ ->
            Just R

        MathToken _ ->
            Just M

        CodeToken _ ->
            Just C

        _ ->
            Nothing


toSymbol2 : Token -> Symbol
toSymbol2 token =
    case token of
        LB _ ->
            L

        RB _ ->
            R

        _ ->
            O
