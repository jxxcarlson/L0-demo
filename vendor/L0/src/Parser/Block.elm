module Parser.Block exposing (BlockType(..), L0BlockE(..))

{-| Types of blocks.

@docs BlockType, L0BlockE

-}

import Either exposing (Either)
import Parser.Expr exposing (Expr)


{-| -}
type L0BlockE
    = L0BlockE
        { name : Maybe String
        , args : List String
        , indent : Int
        , lineNumber : Int
        , numberOfLines : Int
        , id : String
        , blockType : BlockType
        , content : Either String (List Expr)
        , children : List L0BlockE
        , sourceText : String
        }


{-| -}
type BlockType
    = Paragraph
    | OrdinaryBlock (List String)
    | VerbatimBlock (List String)
