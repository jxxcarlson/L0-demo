module Render.ASTTools exposing (exprListToStringList, filterBlocksByArgs, stringValueOfList, tableOfContents, title)

import Maybe.Extra
import Parser.Block exposing (BlockType(..), L0BlockE(..))
import Parser.Expr exposing (Expr(..))
import Tree


title : List (Tree.Tree L0BlockE) -> List L0BlockE
title ast =
    filterBlocksByArgs "title" ast


tableOfContents : List (Tree.Tree L0BlockE) -> List L0BlockE
tableOfContents ast =
    filterBlocksByArgs "heading" ast


filterBlocksByArgs : String -> List (Tree.Tree L0BlockE) -> List L0BlockE
filterBlocksByArgs key ast =
    ast
        |> List.map Tree.flatten
        |> List.concat
        |> List.filter (matchBlock key)


matchBlock : String -> L0BlockE -> Bool
matchBlock key (L0BlockE { blockType }) =
    case blockType of
        Paragraph ->
            False

        OrdinaryBlock args ->
            List.any (String.contains key) args

        VerbatimBlock args ->
            List.any (String.contains key) args


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
