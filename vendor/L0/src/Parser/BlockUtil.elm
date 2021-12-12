module Parser.BlockUtil exposing (l0Empty, toBlock, toL0Block, toL0BlockE)

import Either exposing (Either(..))
import Parser.Block exposing (BlockType(..), L0BlockE(..))
import Parser.Expression
import Tree.Blocks
import Tree.BlocksV


type L0Block
    = L0Block
        { name : Maybe String
        , args : List String
        , indent : Int
        , lineNumber : Int
        , blockType : BlockType
        , content : String
        , children : List L0Block
        }


l0Empty =
    L0BlockE
        { name = Nothing
        , args = []
        , indent = 0
        , lineNumber = 0
        , blockType = Paragraph
        , content = Left "YYY"
        , children = []
        }


toBlock : L0BlockE -> Tree.BlocksV.Block
toBlock (L0BlockE { indent, lineNumber }) =
    { indent = indent, content = "XXX", lineNumber = lineNumber }


toL0BlockE : Tree.BlocksV.Block -> L0BlockE
toL0BlockE block =
    let
        blockType =
            classify block
    in
    case blockType of
        Paragraph ->
            L0BlockE
                { name = Nothing
                , args = []
                , indent = block.indent
                , lineNumber = block.lineNumber
                , content = Right (Parser.Expression.parse_ block.content)
                , blockType = blockType
                , children = []
                }

        OrdinaryBlock args ->
            L0BlockE
                { name = List.head args
                , args = List.drop 1 args
                , indent = block.indent
                , lineNumber = block.lineNumber
                , content = Right (Parser.Expression.parse_ (removeFirstLine block.content))
                , blockType = blockType
                , children = []
                }

        VerbatimBlock args ->
            L0BlockE
                { name = List.head args
                , args = List.drop 1 args
                , indent = block.indent
                , lineNumber = block.lineNumber
                , content = Left (removeFirstLine block.content)
                , blockType = blockType
                , children = []
                }


removeFirstLine : String -> String
removeFirstLine str_ =
    str_ |> String.trim |> String.lines |> List.drop 1 |> String.join "\n"


toL0Block : Tree.BlocksV.Block -> L0Block
toL0Block block =
    let
        blockType =
            classify block
    in
    case blockType of
        Paragraph ->
            L0Block
                { name = Nothing
                , args = []
                , indent = block.indent
                , lineNumber = block.lineNumber
                , content = block.content
                , blockType = blockType
                , children = []
                }

        OrdinaryBlock args ->
            L0Block
                { name = List.head args
                , args = List.drop 1 args
                , indent = block.indent
                , lineNumber = block.lineNumber
                , content = block.content
                , blockType = blockType
                , children = []
                }

        VerbatimBlock args ->
            L0Block
                { name = List.head args
                , args = List.drop 1 args
                , indent = block.indent
                , lineNumber = block.lineNumber
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
