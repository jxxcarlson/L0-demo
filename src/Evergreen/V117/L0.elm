module Evergreen.V117.L0 exposing (..)

import Evergreen.V117.Parser.Block
import Tree


type alias SyntaxTree =
    List (Tree.Tree Evergreen.V117.Parser.Block.ExpressionBlock)
