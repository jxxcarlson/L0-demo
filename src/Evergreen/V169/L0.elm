module Evergreen.V169.L0 exposing (..)

import Evergreen.V169.Parser.Block
import Tree


type alias SyntaxTree =
    List (Tree.Tree Evergreen.V169.Parser.Block.ExpressionBlock)
