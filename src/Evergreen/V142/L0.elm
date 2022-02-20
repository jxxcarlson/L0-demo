module Evergreen.V142.L0 exposing (..)

import Evergreen.V142.Parser.Block
import Tree


type alias SyntaxTree =
    List (Tree.Tree Evergreen.V142.Parser.Block.ExpressionBlock)
