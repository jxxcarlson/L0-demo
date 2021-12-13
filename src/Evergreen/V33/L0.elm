module Evergreen.V33.L0 exposing (..)

import Evergreen.V33.Parser.Block
import Tree


type alias AST =
    List (Tree.Tree Evergreen.V33.Parser.Block.L0BlockE)
