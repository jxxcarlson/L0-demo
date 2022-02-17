module Evergreen.V88.L0 exposing (..)

import Evergreen.V88.Parser.Block
import Tree


type alias SyntaxTree =
    List (Tree.Tree Evergreen.V88.Parser.Block.ExpressionBlock)
