module Evergreen.V48.L0 exposing (..)

import Evergreen.V48.Parser.Block
import Tree


type alias SyntaxTree =
    List (Tree.Tree Evergreen.V48.Parser.Block.ExpressionBlock)
