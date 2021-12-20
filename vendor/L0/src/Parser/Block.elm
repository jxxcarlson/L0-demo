module Parser.Block exposing
    ( BlockType(..)
    , ExpressionBlock(..)
    )

{-| Types of blocks.

@docs BlockType, L0BlockE

-}

import Either exposing (Either)
import Parser.Expr exposing (Expr)


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
        , children : List ExpressionBlock
        , sourceText : String
        }


{-| -}
type BlockType
    = Paragraph
    | OrdinaryBlock (List String)
    | VerbatimBlock (List String)
