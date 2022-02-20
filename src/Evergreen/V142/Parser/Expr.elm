module Evergreen.V142.Parser.Expr exposing (..)

import Evergreen.V142.Parser.Token


type Expr
    = Expr String (List Expr) Evergreen.V142.Parser.Token.Meta
    | Text String Evergreen.V142.Parser.Token.Meta
    | Verbatim String String Evergreen.V142.Parser.Token.Meta
    | Error String
