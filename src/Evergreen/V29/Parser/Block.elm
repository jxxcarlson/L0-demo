module Evergreen.V29.Parser.Block exposing (..)

import Either
import Evergreen.V29.Parser.Expr


type BlockType
    = Paragraph
    | OrdinaryBlock (List String)
    | VerbatimBlock (List String)


type L0BlockE
    = L0BlockE
        { name : Maybe String
        , args : List String
        , indent : Int
        , lineNumber : Int
        , blockType : BlockType
        , content : Either.Either String (List Evergreen.V29.Parser.Expr.Expr)
        , children : List L0BlockE
        }
