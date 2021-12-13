module Render.ASTTools exposing
    ( exprListToStringList
    , filterBlocksByArgs
    , getExprsByName
    , getText
    , matchingIdsInAST
    , stringValueOfList
    , tableOfContents
    , title
    , toExprRecord
    )

import Either exposing (Either(..))
import L0 exposing (AST)
import Maybe.Extra
import Parser.Block exposing (BlockType(..), L0BlockE(..))
import Parser.Expr exposing (Expr(..))
import Tree


getExprsByName : String -> List Expr -> List Expr
getExprsByName name exprs =
    List.filter (matchExpr name) exprs


matchExpr : String -> Expr -> Bool
matchExpr name expr =
    case expr of
        Expr name2 _ _ ->
            name == name2

        Verbatim name2 _ _ ->
            name == name2

        _ ->
            False


matchingIdsInAST : String -> AST -> List String
matchingIdsInAST key ast =
    ast |> List.map Tree.flatten |> List.concat |> List.filterMap (idOfMatchingBlockContent key)


idOfMatchingBlockContent : String -> L0BlockE -> Maybe String
idOfMatchingBlockContent key (L0BlockE { sourceText, id }) =
    if String.contains key sourceText then
        Just id

    else
        Nothing


title : L0.AST -> List L0BlockE
title ast =
    filterBlocksByArgs "title" ast


tableOfContents : L0.AST -> List L0BlockE
tableOfContents ast =
    filterBlocksByArgs "heading" ast


filterBlocksByArgs : String -> L0.AST -> List L0BlockE
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



-- toExprListList : List L0BlockE -> List (List Expr)


toExprRecord : List L0BlockE -> List { content : List Expr, blockType : BlockType }
toExprRecord blocks =
    List.map toExprList_ blocks



-- toExprList_ : L0BlockE -> List Expr


toExprList_ (L0BlockE { blockType, content }) =
    { content = content |> Either.toList |> List.concat, blockType = blockType }
