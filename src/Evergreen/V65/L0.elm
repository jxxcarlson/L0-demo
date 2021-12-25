module Evergreen.V65.L0 exposing (..)

import Evergreen.V65.Parser.Block
import Tree


type alias SyntaxTree =
    List (Tree.Tree Evergreen.V65.Parser.Block.ExpressionBlock)
