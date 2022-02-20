module Evergreen.V167.L0 exposing (..)

import Evergreen.V167.Parser.Block
import Tree


type alias SyntaxTree =
    List (Tree.Tree Evergreen.V167.Parser.Block.ExpressionBlock)
