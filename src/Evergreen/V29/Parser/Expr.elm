module Evergreen.V29.Parser.Expr exposing (..)

import Evergreen.V29.Parser.Token


type Expr
    = Expr String (List Expr) Evergreen.V29.Parser.Token.Meta
    | Text String Evergreen.V29.Parser.Token.Meta
    | Verbatim String String Evergreen.V29.Parser.Token.Meta
    | Error String
