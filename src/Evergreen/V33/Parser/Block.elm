module Evergreen.V33.Parser.Block exposing (..)

import Either
import Evergreen.V33.Parser.Expr


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
        , numberOfLines : Int
        , id : String
        , blockType : BlockType
        , content : Either.Either String (List Evergreen.V33.Parser.Expr.Expr)
        , children : List L0BlockE
        , sourceText : String
        }
