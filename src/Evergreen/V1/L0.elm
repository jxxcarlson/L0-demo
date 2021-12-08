module Evergreen.V1.L0 exposing (..)

import Evergreen.V1.Block.Block
import Tree


type alias AST =
    List (Tree.Tree Evergreen.V1.Block.Block.L0BlockE)
