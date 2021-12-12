module Render.Accumulator exposing
    ( Accumulator
    , make
    , transformAST
    )

import Parser.Block exposing (BlockType(..), L0BlockE(..))
import Render.Vector as Vector exposing (Vector)
import Tree exposing (Tree)


type alias Accumulator =
    { headingIndex : Vector
    , numberedItemIndex : Int
    }


transformAST : List (Tree L0BlockE) -> List (Tree L0BlockE)
transformAST ast =
    ast |> make |> Tuple.second


make : List (Tree L0BlockE) -> ( Accumulator, List (Tree L0BlockE) )
make ast =
    List.foldl (\tree ( acc_, ast_ ) -> transformAccumulateTree tree acc_ |> mapper ast_) ( init 4, [] ) ast
        |> (\( acc_, ast_ ) -> ( acc_, List.reverse ast_ ))


init : Int -> Accumulator
init k =
    { headingIndex = Vector.init k
    , numberedItemIndex = 0
    }


mapper ast_ ( acc_, tree_ ) =
    ( acc_, tree_ :: ast_ )


transformAccumulateTree : Tree L0BlockE -> Accumulator -> ( Accumulator, Tree L0BlockE )
transformAccumulateTree tree acc =
    let
        transformer : Accumulator -> L0BlockE -> ( Accumulator, L0BlockE )
        transformer =
            -- \acc_ block_ -> ( updateAccumulator block_ acc_, transformBlock acc_ block_ )
            \acc_ block_ ->
                let
                    newAcc =
                        updateAccumulator block_ acc_
                in
                ( newAcc, transformBlock newAcc block_ )
    in
    Tree.mapAccumulate transformer acc tree


transformBlock : Accumulator -> L0BlockE -> L0BlockE
transformBlock acc ((L0BlockE { args, blockType, children, content, indent, lineNumber, name }) as block) =
    case blockType of
        OrdinaryBlock [ "heading", level ] ->
            L0BlockE { args = args ++ [ Vector.toString acc.headingIndex ], blockType = blockType, children = children, content = content, indent = indent, lineNumber = lineNumber, name = name }

        OrdinaryBlock [ "numbered" ] ->
            L0BlockE { args = args ++ [ String.fromInt acc.numberedItemIndex ], blockType = blockType, children = children, content = content, indent = indent, lineNumber = lineNumber, name = name }

        _ ->
            block


updateAccumulator : L0BlockE -> Accumulator -> Accumulator
updateAccumulator ((L0BlockE { blockType, content }) as block) accumulator =
    case blockType of
        OrdinaryBlock [ "heading", level ] ->
            let
                headingIndex =
                    Vector.increment (String.toInt level |> Maybe.withDefault 0 |> (\x -> x - 1)) accumulator.headingIndex
            in
            { accumulator | headingIndex = headingIndex, numberedItemIndex = 0 }

        OrdinaryBlock [ "numbered" ] ->
            let
                numberedItemIndex =
                    accumulator.numberedItemIndex + 1
            in
            { accumulator | numberedItemIndex = numberedItemIndex }

        _ ->
            { accumulator | numberedItemIndex = 0 }
