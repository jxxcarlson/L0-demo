module Evergreen.V31.L0 exposing (..)

import Evergreen.V31.Parser.Block
import Tree


type alias AST =
    List (Tree.Tree Evergreen.V31.Parser.Block.L0BlockE)
