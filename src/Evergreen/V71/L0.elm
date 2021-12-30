module Evergreen.V71.L0 exposing (..)

import Evergreen.V71.Parser.Block
import Tree


type alias SyntaxTree =
    List (Tree.Tree Evergreen.V71.Parser.Block.ExpressionBlock)
