module Parser.BlockUtil exposing
    ( empty
    , getMessages
    , l0Empty
    , toBlock
    , toBlockFromIntermediateBlock
    , toExpressionBlock
    , toExpressionBlockFromIntermediateBlock
    , toIntermediateBlock
    , toL0Block
    )

import Either exposing (Either(..))
import Parser.Block exposing (BlockType(..), ExpressionBlock(..), IntermediateBlock(..))
import Parser.Expr exposing (Expr)
import Parser.Expression
import Parser.Helpers as Helpers
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


empty =
    IntermediateBlock
        { name = Nothing
        , args = []
        , indent = 0
        , lineNumber = 0
        , id = "0"
        , numberOfLines = 0
        , blockType = Paragraph
        , content = ""
        , messages = []
        , children = []
        , sourceText = "YYY"
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
        , messages = []
        , children = []
        , sourceText = "YYY"
        }


getMessages : ExpressionBlock -> List String
getMessages ((ExpressionBlock { messages }) as block) =
    messages


toBlock : ExpressionBlock -> Tree.BlocksV.Block
toBlock (ExpressionBlock { indent, lineNumber, numberOfLines }) =
    { indent = indent, content = "XXX", lineNumber = lineNumber, numberOfLines = numberOfLines }


toBlockFromIntermediateBlock : IntermediateBlock -> Tree.BlocksV.Block
toBlockFromIntermediateBlock (IntermediateBlock { indent, lineNumber, numberOfLines }) =
    { indent = indent, content = "XXX", lineNumber = lineNumber, numberOfLines = numberOfLines }


toExpressionBlockFromIntermediateBlock : IntermediateBlock -> ExpressionBlock
toExpressionBlockFromIntermediateBlock (IntermediateBlock { name, args, indent, lineNumber, id, blockType, content, messages, children, sourceText }) =
    ExpressionBlock
        { name = name
        , args = args
        , indent = indent
        , lineNumber = lineNumber
        , numberOfLines = List.length (String.lines content)
        , id = id
        , blockType = blockType
        , content = mapContent lineNumber blockType content
        , messages = messages
        , children = List.map toExpressionBlockFromIntermediateBlock children
        , sourceText = sourceText
        }


toExpressionBlock : Int -> Tree.BlocksV.Block -> ExpressionBlock
toExpressionBlock lineNumber block =
    let
        blockType =
            classify block

        state =
            Parser.Expression.parseToState lineNumber block.content
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
                , content = Right (Parser.Expression.parse block.lineNumber block.content)
                , messages = state.messages
                , blockType = blockType
                , children = []
                , sourceText = block.content
                }

        OrdinaryBlock args ->
            let
                ( firstLine, rawContent_ ) =
                    split block.content

                messages =
                    if rawContent_ == "" then
                        ("Write something below the block header (" ++ String.replace "| " "" firstLine ++ ")") :: state.messages

                    else
                        state.messages

                rawContent =
                    if rawContent_ == "" then
                        firstLine ++ "\n[red Write something below this block header (" ++ String.replace "| " "" firstLine ++ ")]"

                    else
                        rawContent_
            in
            ExpressionBlock
                { name = List.head args
                , args = List.drop 1 args
                , indent = block.indent
                , lineNumber = block.lineNumber
                , id = String.fromInt block.lineNumber
                , numberOfLines = block.numberOfLines
                , content = Right (Parser.Expression.parse lineNumber rawContent)

                --, content = Right state.committed
                , messages = messages
                , blockType = blockType
                , children = []
                , sourceText = block.content
                }

        VerbatimBlock args ->
            let
                ( firstLine, rawContent ) =
                    split block.content

                messages =
                    case blockType of
                        VerbatimBlock [ "math" ] ->
                            if String.endsWith "$$" rawContent then
                                state.messages

                            else
                                Helpers.prependMessage lineNumber "You need to close this math expression with '$$'" state.messages

                        VerbatimBlock [ "code" ] ->
                            if String.startsWith "```" firstLine && not (String.endsWith "```" rawContent) then
                                Helpers.prependMessage lineNumber "You need to close this code block with triple backticks" state.messages

                            else
                                state.messages

                        _ ->
                            state.messages

                content =
                    if blockType == VerbatimBlock [ "code" ] then
                        Left (String.replace "```" "" rawContent)

                    else
                        Left rawContent
            in
            ExpressionBlock
                { name = List.head args
                , args = List.drop 1 args
                , indent = block.indent
                , lineNumber = block.lineNumber
                , id = String.fromInt block.lineNumber
                , numberOfLines = block.numberOfLines
                , content = content

                --, content = Right state.committed
                , messages = messages
                , blockType = blockType
                , children = []
                , sourceText = block.content
                }


mapContent : Int -> BlockType -> String -> Either String (List Expr)
mapContent lineNumber blockType content =
    case blockType of
        Paragraph ->
            Right (Parser.Expression.parse lineNumber content)

        OrdinaryBlock args ->
            let
                ( firstLine, rawContent_ ) =
                    split content

                --messages =
                --    if rawContent_ == "" then
                --        ("Write something below the block header (" ++ String.replace "| " "" firstLine ++ ")") :: state.messages
                --
                --    else
                --        state.messages
                rawContent =
                    if rawContent_ == "" then
                        firstLine ++ "\n[red Write something below this block header (" ++ String.replace "| " "" firstLine ++ ")]"

                    else
                        rawContent_
            in
            Right (Parser.Expression.parse lineNumber content)

        VerbatimBlock args ->
            let
                ( firstLine, rawContent ) =
                    split content

                --messages =
                --    case blockType of
                --        VerbatimBlock [ "math" ] ->
                --            if String.endsWith "$$" rawContent then
                --                state.messages
                --
                --            else
                --                "You need to close this math expression with '$$'" :: state.messages
                --
                --        VerbatimBlock [ "code" ] ->
                --            if String.endsWith "```" rawContent then
                --                state.messages
                --
                --            else
                --                "You need to close this code block with triple backticks" :: []
                --
                --        _ ->
                --            state.messages
                content_ =
                    if blockType == VerbatimBlock [ "code" ] then
                        Left (String.replace "```" "" content)

                    else
                        Left content
            in
            content_


bareBlockNames =
    [ "makeTableOfContents" ]


toIntermediateBlock : Tree.BlocksV.Block -> IntermediateBlock
toIntermediateBlock block =
    let
        blockType =
            classify block

        state =
            Parser.Expression.parseToState block.lineNumber block.content
    in
    case blockType of
        Paragraph ->
            IntermediateBlock
                { name = Nothing
                , args = []
                , indent = block.indent
                , lineNumber = block.lineNumber
                , id = String.fromInt block.lineNumber
                , numberOfLines = block.numberOfLines
                , content = block.content

                --, content = Right state.committed
                , messages = state.messages
                , blockType = blockType
                , children = []
                , sourceText = block.content
                }

        OrdinaryBlock args ->
            let
                ( firstLine, rawContent_ ) =
                    split block.content

                messages =
                    if rawContent_ == "" && not (List.member (List.head args |> Maybe.withDefault "!!") bareBlockNames) then
                        Helpers.prependMessage block.lineNumber ("Write something below the block header (" ++ String.replace "| " "" firstLine ++ ")") state.messages

                    else
                        state.messages

                rawContent =
                    if rawContent_ == "" && not (List.member (List.head args |> Maybe.withDefault "!!") bareBlockNames) then
                        firstLine ++ "\n[red Write something below this block header (" ++ String.replace "| " "" firstLine ++ ")]"

                    else
                        rawContent_
            in
            IntermediateBlock
                { name = List.head args
                , args = List.drop 1 args
                , indent = block.indent
                , lineNumber = block.lineNumber
                , id = String.fromInt block.lineNumber
                , numberOfLines = block.numberOfLines
                , content = rawContent

                --, content = Right state.committed
                , messages = messages
                , blockType = blockType
                , children = []
                , sourceText = block.content
                }

        VerbatimBlock args ->
            let
                ( firstLine, rawContent ) =
                    split block.content

                messages =
                    case blockType of
                        VerbatimBlock [ "math" ] ->
                            if String.endsWith "$$" rawContent then
                                state.messages

                            else
                                Helpers.prependMessage block.lineNumber "You need to close this math expression with '$$'" state.messages

                        VerbatimBlock [ "code" ] ->
                            if String.startsWith "```" firstLine && not (String.endsWith "```" rawContent) then
                                Helpers.prependMessage block.lineNumber "You need to close this code block with triple backticks" state.messages

                            else
                                state.messages

                        _ ->
                            state.messages

                content =
                    if blockType == VerbatimBlock [ "code" ] then
                        String.replace "```" "" rawContent

                    else
                        rawContent
            in
            IntermediateBlock
                { name = List.head args
                , args = List.drop 1 args
                , indent = block.indent
                , lineNumber = block.lineNumber
                , id = String.fromInt block.lineNumber
                , numberOfLines = block.numberOfLines
                , content = content

                --, content = Right state.committed
                , messages = messages
                , blockType = blockType
                , children = []
                , sourceText = block.content
                }


{-| Split into first line and all the rest
-}
split : String -> ( String, String )
split str_ =
    let
        lines =
            str_ |> String.trim |> String.lines
    in
    ( List.head lines |> Maybe.withDefault "", lines |> List.drop 1 |> String.join "\n" )


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

    else if String.left 3 str_ == "```" then
        VerbatimBlock [ "code" ]

    else
        Paragraph
