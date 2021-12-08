module ASTTools exposing (exprListToStringList, stringValueOfList)

import Maybe.Extra
import Parser.Expression exposing (Expr(..))


exprListToStringList : List Expr -> List String
exprListToStringList exprList =
    List.map getText exprList
        |> Maybe.Extra.values
        |> List.map String.trim
        |> List.filter (\s -> s /= "")


getText : Expr -> Maybe String
getText text =
    case text of
        Text str _ ->
            Just str

        Verbatim _ str _ ->
            Just (String.replace "`" "" str)

        Expr _ expressions _ ->
            List.map getText expressions |> Maybe.Extra.values |> String.join " " |> Just

        _ ->
            Nothing


stringValueOfList : List Expr -> String
stringValueOfList textList =
    String.join " " (List.map stringValue textList)


stringValue : Expr -> String
stringValue text =
    case text of
        Text str _ ->
            str

        Expr _ textList _ ->
            String.join " " (List.map stringValue textList)

        Verbatim _ str _ ->
            str

        Error str ->
            str
