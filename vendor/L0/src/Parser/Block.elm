module Parser.Block exposing
    ( BlockType(..)
    , ExpressionBlock(..), IntermediateBlock(..)
    )

{-| Types of blocks.

@docs BlockType, L0BlockE

-}

import Either exposing (Either(..))
import Parser.Expr exposing (Expr)
import Parser.Expression


{-| -}
type ExpressionBlock
    = ExpressionBlock
        { name : Maybe String
        , args : List String
        , indent : Int
        , lineNumber : Int
        , numberOfLines : Int
        , id : String
        , blockType : BlockType
        , content : Either String (List Expr)
        , messages : List String
        , children : List ExpressionBlock
        , sourceText : String
        }


{-| -}
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


{-| -}
type BlockType
    = Paragraph
    | OrdinaryBlock (List String)
    | VerbatimBlock (List String)
