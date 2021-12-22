module Parser.Repl exposing (..)

import Parser.Expression
import Parser.Simple


p str =
    str |> Parser.Expression.parse |> List.map Parser.Simple.simplify
