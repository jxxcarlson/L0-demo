module Evergreen.V65.Parser.Expr exposing (..)

import Evergreen.V65.Parser.Token


type Expr
    = Expr String (List Expr) Evergreen.V65.Parser.Token.Meta
    | Text String Evergreen.V65.Parser.Token.Meta
    | Verbatim String String Evergreen.V65.Parser.Token.Meta
    | Error String
