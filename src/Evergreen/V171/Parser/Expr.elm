module Evergreen.V171.Parser.Expr exposing (..)

import Evergreen.V171.Parser.Token


type Expr
    = Expr String (List Expr) Evergreen.V171.Parser.Token.Meta
    | Text String Evergreen.V171.Parser.Token.Meta
    | Verbatim String String Evergreen.V171.Parser.Token.Meta
    | Error String
