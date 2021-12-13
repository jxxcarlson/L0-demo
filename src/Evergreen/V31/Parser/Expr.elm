module Evergreen.V31.Parser.Expr exposing (..)

import Evergreen.V31.Parser.Token


type Expr
    = Expr String (List Expr) Evergreen.V31.Parser.Token.Meta
    | Text String Evergreen.V31.Parser.Token.Meta
    | Verbatim String String Evergreen.V31.Parser.Token.Meta
    | Error String
