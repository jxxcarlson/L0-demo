module L0 exposing (AST, parse)

{-| A Parser for the experimental L0 module. See the app folder to see how it is used.
The Render folder in app could have been included with the parser. However, this way
users are free to design their own renderer.

Since this package is still experimental (but needed in various test projects).
The documentation is skimpy.

@docs AST, parse

-}

import Parser.Block
import Parser.BlockUtil
import Tree exposing (Tree)
import Tree.BlocksV
import Tree.Build exposing (Error)


{-| -}
type alias AST =
    List (Tree Parser.Block.L0BlockE)


isVerbatimLine : String -> Bool
isVerbatimLine str =
    String.left 2 str == "||"


{-| -}
parse : String -> AST
parse sourceText =
    sourceText
        |> Tree.BlocksV.fromStringAsParagraphs isVerbatimLine
        |> Tree.Build.forestFromBlocks Parser.BlockUtil.l0Empty Parser.BlockUtil.toL0BlockE Parser.BlockUtil.toBlock
        |> Result.withDefault []


b =
    Tree.BlocksV.fromStringAsParagraphs isVerbatimLine
