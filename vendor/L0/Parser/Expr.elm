module Parser.Expr exposing (Expr(..))

{-| The syntax tree for the parser is of type Expr.

@docs Expr

-}

import Parser.Token exposing (Meta)


{-| -}
type Expr
    = Expr String (List Expr) Meta
    | Text String Meta
    | Verbatim String String Meta
    | Error String
