module Render.Text exposing (fromExpr, idem, print)

import Parser.Expr exposing (Expr(..))
import Parser.Expression as Expression


idem : String -> String
idem str =
    str
        |> Expression.parseToState
        |> .committed
        |> print


print : List Expr -> String
print expressions =
    List.map fromExpr expressions |> String.join ""


fromExpr : Expr -> String
fromExpr expr =
    case expr of
        Expr name expressions _ ->
            "[" ++ name ++ (List.map fromExpr expressions |> String.join "") ++ "]"

        Text str _ ->
            str

        Verbatim name str _ ->
            case name of
                "math" ->
                    "$" ++ str ++ "$"

                "code" ->
                    "`" ++ str ++ "`"

                _ ->
                    "error: verbatim " ++ name ++ " not recognized"

        Error str ->
            "Error (" ++ str ++ ")"
