module L0 exposing (renderFromString1, renderFromString2, renderFromString3)

import Block.Block
import Element exposing (Element)
import Render.Block
import Render.Msg exposing (MarkupMsg)
import Render.Settings exposing (Settings)
import Tree exposing (Tree)
import Tree.Blocks exposing (Block)
import Tree.BlocksV
import Tree.Build exposing (Error)


renderFromString1 : Int -> Settings -> String -> List (Element MarkupMsg)
renderFromString1 count settings sourceText =
    sourceText
        |> Tree.BlocksV.fromStringAsParagraphs isVerbatimLine
        |> List.map Block.Block.toL0BlockE
        |> List.map (Render.Block.render count settings)


isVerbatimLine : String -> Bool
isVerbatimLine str =
    String.left 2 str == "||"


renderFromString2 : Int -> Settings -> String -> Result Error (List (Element MarkupMsg))
renderFromString2 count settings sourceText =
    sourceText
        |> Tree.BlocksV.fromStringAsParagraphs isVerbatimLine
        |> Tree.Build.forestFromBlocks Block.Block.l0Empty Block.Block.toL0BlockE Block.Block.toBlock
        |> Result.map (List.map (Tree.map (Render.Block.render count settings)))
        |> Result.map (List.map Tree.flatten >> List.concat)


renderFromString3 : Int -> Settings -> String -> Result Error (List (Element MarkupMsg))
renderFromString3 count settings sourceText =
    sourceText
        |> Tree.BlocksV.fromStringAsParagraphs isVerbatimLine
        |> Tree.Build.forestFromBlocks Block.Block.l0Empty Block.Block.toL0BlockE Block.Block.toBlock
        |> Result.map (List.map (Tree.map (Render.Block.render count settings)))
        |> Result.map (List.map unravel)


unravel : Tree (Element MarkupMsg) -> Element MarkupMsg
unravel tree =
    let
        children =
            Tree.children tree
    in
    if List.isEmpty children then
        Tree.label tree

    else
        Element.column []
            [ Tree.label tree
            , Element.column [ Render.Settings.leftIndentation ] (List.map unravel children)
            ]
