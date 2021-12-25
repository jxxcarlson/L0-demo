module Evergreen.V65.Parser.Block exposing (..)

import Either
import Evergreen.V65.Parser.Expr


type BlockType
    = Paragraph
    | OrdinaryBlock (List String)
    | VerbatimBlock (List String)


type ExpressionBlock
    = ExpressionBlock
        { name : Maybe String
        , args : List String
        , indent : Int
        , lineNumber : Int
        , numberOfLines : Int
        , id : String
        , blockType : BlockType
        , content : Either.Either String (List Evergreen.V65.Parser.Expr.Expr)
        , messages : List String
        , children : List ExpressionBlock
        , sourceText : String
        }
