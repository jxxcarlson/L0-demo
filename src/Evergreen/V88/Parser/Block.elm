module Evergreen.V88.Parser.Block exposing (..)

import Either
import Evergreen.V88.Parser.Expr


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
        , content : Either.Either String (List Evergreen.V88.Parser.Expr.Expr)
        , messages : List String
        , children : List ExpressionBlock
        , sourceText : String
        }


type IntermediateBlock
    = IntermediateBlock
        { name : Maybe String
        , args : List String
        , indent : Int
        , lineNumber : Int
        , numberOfLines : Int
        , id : String
        , blockType : BlockType
        , content : String
        , messages : List String
        , children : List IntermediateBlock
        , sourceText : String
        }
