module Evergreen.V29.L0 exposing (..)

import Evergreen.V29.Parser.Block
import Tree


type alias AST =
    List (Tree.Tree Evergreen.V29.Parser.Block.L0BlockE)
