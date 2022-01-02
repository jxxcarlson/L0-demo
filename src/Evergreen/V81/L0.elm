module Evergreen.V81.L0 exposing (..)

import Evergreen.V81.Parser.Block
import Tree


type alias SyntaxTree =
    List (Tree.Tree Evergreen.V81.Parser.Block.ExpressionBlock)
