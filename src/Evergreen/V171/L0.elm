module Evergreen.V171.L0 exposing (..)

import Evergreen.V171.Parser.Block
import Tree


type alias SyntaxTree =
    List (Tree.Tree Evergreen.V171.Parser.Block.ExpressionBlock)
