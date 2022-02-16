module Compiler.ASTTools exposing
    ( exprListToStringList
    , extractTextFromSyntaxTreeByKey
    , filterBlocksByArgs
    , filterBlocksOnName
    , filterExpressionsOnName
    , getText
    , matchingIdsInAST
    , stringValueOfList
    , tableOfContents
    , title
    , toExprRecord
    )

import Either exposing (Either(..))
import L0 exposing (SyntaxTree)
import Maybe.Extra
import Parser.Block exposing (BlockType(..), ExpressionBlock(..))
import Parser.Expr exposing (Expr(..))
import Tree


filterExpressionsOnName : String -> List Expr -> List Expr
filterExpressionsOnName name exprs =
    List.filter (matchExprOnName name) exprs


filterBlocksOnName : String -> List ExpressionBlock -> List ExpressionBlock
filterBlocksOnName name blocks =
    List.filter (matchBlockOnName name) blocks


matchBlockOnName : String -> ExpressionBlock -> Bool
matchBlockOnName key (ExpressionBlock { name }) =
    Just key == name


matchExprOnName : String -> Expr -> Bool
matchExprOnName name expr =
    case expr of
        Expr name2 _ _ ->
            name == name2

        Verbatim name2 _ _ ->
            name == name2

        _ ->
            False


matchingIdsInAST : String -> SyntaxTree -> List String
matchingIdsInAST key ast =
    ast |> List.map Tree.flatten |> List.concat |> List.filterMap (idOfMatchingBlockContent key)


idOfMatchingBlockContent : String -> ExpressionBlock -> Maybe String
idOfMatchingBlockContent key (ExpressionBlock { sourceText, id }) =
    if String.contains key sourceText then
        Just id

    else
        Nothing


title : L0.SyntaxTree -> List ExpressionBlock
title ast =
    filterBlocksByArgs "title" ast


extractTextFromSyntaxTreeByKey key syntaxTree =
    syntaxTree |> filterBlocksByArgs key |> expressionBlockToText


tableOfContents : L0.SyntaxTree -> List ExpressionBlock
tableOfContents ast =
    filterBlocksByArgs "heading" ast


filterBlocksByArgs : String -> L0.SyntaxTree -> List ExpressionBlock
filterBlocksByArgs key ast =
    ast
        |> List.map Tree.flatten
        |> List.concat
        |> List.filter (matchBlock key)


matchBlock : String -> ExpressionBlock -> Bool
matchBlock key (ExpressionBlock { blockType }) =
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


expressionBlockToText : List ExpressionBlock -> String
expressionBlockToText =
    toExprRecord >> List.map .content >> List.concat >> List.filterMap getText >> String.join " "



-- toExprListList : List L0BlockE -> List (List Expr)


toExprRecord : List ExpressionBlock -> List { content : List Expr, blockType : BlockType }
toExprRecord blocks =
    List.map toExprList_ blocks



-- toExprList_ : L0BlockE -> List Expr


toExprList_ (ExpressionBlock { blockType, content }) =
    { content = content |> Either.toList |> List.concat, blockType = blockType }
