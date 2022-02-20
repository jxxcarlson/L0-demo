module Evergreen.V168.L0 exposing (..)

import Evergreen.V168.Parser.Block
import Tree


type alias SyntaxTree =
    List (Tree.Tree Evergreen.V168.Parser.Block.ExpressionBlock)
