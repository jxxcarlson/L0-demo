module Parser.Repl exposing (..)

import Parser.Expression
import Parser.Simple


p str =
    str |> Parser.Expression.parse_ |> List.map Parser.Simple.simplify
