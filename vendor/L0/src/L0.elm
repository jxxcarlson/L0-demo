module L0 exposing
    ( AST
    , parse
    )

{-| A Parser for the experimental L0 module. See the app folder to see how it is used.
The Render folder in app could have been included with the parser. However, this way
users are free to design their own renderer.

Since this package is still experimental (but needed in various test projects).
The documentation is skimpy.

@docs AST, parser

-}

import Block.Block
import Tree exposing (Tree)
import Tree.BlocksV
import Tree.Build exposing (Error)


{-| -}
type alias AST =
    List (Tree Block.Block.L0BlockE)


isVerbatimLine : String -> Bool
isVerbatimLine str =
    String.left 2 str == "||"


{-| -}
parse : String -> AST
parse sourceText =
    sourceText
        |> Tree.BlocksV.fromStringAsParagraphs isVerbatimLine
        |> Tree.Build.forestFromBlocks Block.Block.l0Empty Block.Block.toL0BlockE Block.Block.toBlock
        |> Result.withDefault []
