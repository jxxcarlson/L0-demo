module Parser.BlockUtil exposing (l0Empty, toBlock, toExpressionBlock, toL0Block)

import Either exposing (Either(..))
import Parser.Block exposing (BlockType(..), ExpressionBlock(..))
import Parser.Expression
import Tree.BlocksV


type Block
    = Block
        { name : Maybe String
        , args : List String
        , indent : Int
        , lineNumber : Int
        , numberOfLines : Int
        , blockType : BlockType
        , content : String
        , children : List Block
        }


l0Empty =
    ExpressionBlock
        { name = Nothing
        , args = []
        , indent = 0
        , lineNumber = 0
        , id = "0"
        , numberOfLines = 0
        , blockType = Paragraph
        , content = Left "YYY"
        , children = []
        , sourceText = "YYY"
        }


toBlock : ExpressionBlock -> Tree.BlocksV.Block
toBlock (ExpressionBlock { indent, lineNumber, numberOfLines }) =
    { indent = indent, content = "XXX", lineNumber = lineNumber, numberOfLines = numberOfLines }


toExpressionBlock : Tree.BlocksV.Block -> ExpressionBlock
toExpressionBlock block =
    let
        blockType =
            classify block
    in
    case blockType of
        Paragraph ->
            ExpressionBlock
                { name = Nothing
                , args = []
                , indent = block.indent
                , lineNumber = block.lineNumber
                , id = String.fromInt block.lineNumber
                , numberOfLines = block.numberOfLines
                , content = Right (Parser.Expression.parse block.content)
                , blockType = blockType
                , children = []
                , sourceText = block.content
                }

        OrdinaryBlock args ->
            ExpressionBlock
                { name = List.head args
                , args = List.drop 1 args
                , indent = block.indent
                , lineNumber = block.lineNumber
                , id = String.fromInt block.lineNumber
                , numberOfLines = block.numberOfLines
                , content = Right (Parser.Expression.parse (removeFirstLine block.content))
                , blockType = blockType
                , children = []
                , sourceText = block.content
                }

        VerbatimBlock args ->
            ExpressionBlock
                { name = List.head args
                , args = List.drop 1 args
                , indent = block.indent
                , lineNumber = block.lineNumber
                , id = String.fromInt block.lineNumber
                , numberOfLines = block.numberOfLines
                , content = Left (removeFirstLine block.content)
                , blockType = blockType
                , children = []
                , sourceText = block.content
                }


removeFirstLine : String -> String
removeFirstLine str_ =
    str_ |> String.trim |> String.lines |> List.drop 1 |> String.join "\n"


toL0Block : Tree.BlocksV.Block -> Block
toL0Block block =
    let
        blockType =
            classify block
    in
    case blockType of
        Paragraph ->
            Block
                { name = Nothing
                , args = []
                , indent = block.indent
                , lineNumber = block.lineNumber
                , numberOfLines = block.numberOfLines
                , content = block.content
                , blockType = blockType
                , children = []
                }

        OrdinaryBlock args ->
            Block
                { name = List.head args
                , args = List.drop 1 args
                , indent = block.indent
                , lineNumber = block.lineNumber
                , numberOfLines = block.numberOfLines
                , content = block.content
                , blockType = blockType
                , children = []
                }

        VerbatimBlock args ->
            Block
                { name = List.head args
                , args = List.drop 1 args
                , indent = block.indent
                , lineNumber = block.lineNumber
                , numberOfLines = block.numberOfLines
                , content = block.content
                , blockType = blockType
                , children = []
                }


classify : Tree.BlocksV.Block -> BlockType
classify block =
    let
        str_ =
            String.trim block.content
    in
    if String.left 2 str_ == "||" then
        VerbatimBlock (str_ |> String.lines |> List.head |> Maybe.withDefault "" |> String.words |> List.drop 1)

    else if String.left 1 str_ == "|" then
        OrdinaryBlock (str_ |> String.lines |> List.head |> Maybe.withDefault "" |> String.words |> List.drop 1)

    else if String.left 2 str_ == "$$" then
        VerbatimBlock [ "math" ]

    else
        Paragraph
