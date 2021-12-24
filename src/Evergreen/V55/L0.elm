module Evergreen.V55.L0 exposing (..)

import Evergreen.V55.Parser.Block
import Tree


type alias SyntaxTree =
    List (Tree.Tree Evergreen.V55.Parser.Block.ExpressionBlock)
