module Render.Acc exposing
    ( Accumulator
    , make
    , transformST
    )

import Dict exposing (Dict)
import Either exposing (Either(..))
import Parser.Block exposing (BlockType(..), ExpressionBlock(..))
import Parser.Expr exposing (Expr(..))
import Render.Lambda as Lambda exposing (Lambda)
import Render.Vector as Vector exposing (Vector)
import Tree exposing (Tree)


type alias Accumulator =
    { headingIndex : Vector
    , numberedItemIndex : Int
    , equationIndex : Int
    , environment : Dict String Lambda
    }


getLambda : String -> Dict String ( List String, Expr ) -> Maybe { name : String, args : List String, expr : Expr }
getLambda name environment =
    Dict.get name environment |> Maybe.map (\( args, expr ) -> { name = name, args = args, expr = expr })


transformST : List (Tree ExpressionBlock) -> List (Tree ExpressionBlock)
transformST ast =
    ast |> make |> Tuple.second


make : List (Tree ExpressionBlock) -> ( Accumulator, List (Tree ExpressionBlock) )
make ast =
    List.foldl (\tree ( acc_, ast_ ) -> transformAccumulateTree tree acc_ |> mapper ast_) ( init 4, [] ) ast
        |> (\( acc_, ast_ ) -> ( acc_, List.reverse ast_ ))


init : Int -> Accumulator
init k =
    { headingIndex = Vector.init k
    , numberedItemIndex = 0
    , equationIndex = 0
    , environment = Dict.empty
    }


mapper ast_ ( acc_, tree_ ) =
    ( acc_, tree_ :: ast_ )


transformAccumulateTree : Tree ExpressionBlock -> Accumulator -> ( Accumulator, Tree ExpressionBlock )
transformAccumulateTree tree acc =
    let
        transformer : Accumulator -> ExpressionBlock -> ( Accumulator, ExpressionBlock )
        transformer =
            \acc_ block_ ->
                let
                    newAcc =
                        updateAccumulator block_ acc_
                in
                ( newAcc, transformBlock newAcc block_ )
    in
    Tree.mapAccumulate transformer acc tree


transformBlock : Accumulator -> ExpressionBlock -> ExpressionBlock
transformBlock acc ((ExpressionBlock { args, blockType, children, content, messages, indent, lineNumber, numberOfLines, name, id, sourceText }) as block) =
    case blockType of
        OrdinaryBlock [ "heading", level ] ->
            ExpressionBlock
                { args = args ++ [ Vector.toString acc.headingIndex ]
                , blockType = blockType
                , children = children
                , content = content
                , messages = messages
                , indent = indent
                , lineNumber = lineNumber
                , numberOfLines = numberOfLines
                , name = name
                , id = id
                , sourceText = sourceText
                }

        OrdinaryBlock [ "numbered" ] ->
            ExpressionBlock
                { args = args ++ [ String.fromInt acc.numberedItemIndex ]
                , blockType = blockType
                , children = children
                , content = content
                , messages = messages
                , indent = indent
                , lineNumber = lineNumber
                , numberOfLines = numberOfLines
                , name = name
                , id = id
                , sourceText = sourceText
                }

        VerbatimBlock [ "equation" ] ->
            ExpressionBlock
                { args = args ++ [ String.fromInt acc.equationIndex ]
                , blockType = blockType
                , children = children
                , content = content
                , messages = messages
                , indent = indent
                , lineNumber = lineNumber
                , numberOfLines = numberOfLines
                , name = name
                , id = id
                , sourceText = sourceText
                }

        VerbatimBlock [ "aligned" ] ->
            ExpressionBlock
                { args = args ++ [ String.fromInt acc.equationIndex ]
                , blockType = blockType
                , children = children
                , content = content
                , messages = messages
                , indent = indent
                , lineNumber = lineNumber
                , numberOfLines = numberOfLines
                , name = name
                , id = id
                , sourceText = sourceText
                }

        _ ->
            expand acc.environment block


expand : Dict String Lambda -> ExpressionBlock -> ExpressionBlock
expand dict ((ExpressionBlock { args, blockType, children, content, messages, indent, lineNumber, numberOfLines, name, id, sourceText }) as block) =
    ExpressionBlock
        { args = args
        , blockType = blockType
        , children = children
        , content = Either.map (List.map (Lambda.expand dict)) content
        , messages = messages
        , indent = indent
        , lineNumber = lineNumber
        , numberOfLines = numberOfLines
        , name = name
        , id = id
        , sourceText = sourceText
        }


updateAccumulator : ExpressionBlock -> Accumulator -> Accumulator
updateAccumulator ((ExpressionBlock { blockType, content }) as block) accumulator =
    case blockType of
        -- provide numbering for sections
        OrdinaryBlock [ "heading", level ] ->
            let
                headingIndex =
                    Vector.increment (String.toInt level |> Maybe.withDefault 0 |> (\x -> x - 1)) accumulator.headingIndex
            in
            { accumulator | headingIndex = headingIndex, numberedItemIndex = 0 }

        -- provide numbering for lists
        OrdinaryBlock [ "numbered" ] ->
            let
                numberedItemIndex =
                    accumulator.numberedItemIndex + 1
            in
            { accumulator | numberedItemIndex = numberedItemIndex }

        -- provide for numbering of equations
        VerbatimBlock [ "equation" ] ->
            let
                equationIndex =
                    accumulator.equationIndex + 1
            in
            { accumulator | equationIndex = equationIndex }

        VerbatimBlock [ "aligned" ] ->
            let
                equationIndex =
                    accumulator.equationIndex + 1
            in
            { accumulator | equationIndex = equationIndex }

        -- insert definitions of lambdas
        OrdinaryBlock [ "defs" ] ->
            case content of
                Left _ ->
                    accumulator

                Right exprs ->
                    { accumulator | environment = List.foldl (\lambda dict -> Lambda.insert (Lambda.extract lambda) dict) accumulator.environment exprs }

        _ ->
            { accumulator | numberedItemIndex = 0 }
