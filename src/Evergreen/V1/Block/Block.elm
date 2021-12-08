module Evergreen.V1.Block.Block exposing (..)

import Either
import Evergreen.V1.Parser.Expression


type BlockType
    = Paragraph
    | OrdinaryBlock (List String)
    | VerbatimBlock (List String)


type L0BlockE
    = L0BlockE
        { name : Maybe String
        , args : List String
        , indent : Int
        , blockType : BlockType
        , content : Either.Either String (List Evergreen.V1.Parser.Expression.Expr)
        , children : List L0BlockE
        }
