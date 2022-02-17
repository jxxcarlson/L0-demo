module Evergreen.V88.Parser.Expr exposing (..)

import Evergreen.V88.Parser.Token


type Expr
    = Expr String (List Expr) Evergreen.V88.Parser.Token.Meta
    | Text String Evergreen.V88.Parser.Token.Meta
    | Verbatim String String Evergreen.V88.Parser.Token.Meta
    | Error String
