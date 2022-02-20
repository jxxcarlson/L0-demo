module Evergreen.V168.Parser.Expr exposing (..)

import Evergreen.V168.Parser.Token


type Expr
    = Expr String (List Expr) Evergreen.V168.Parser.Token.Meta
    | Text String Evergreen.V168.Parser.Token.Meta
    | Verbatim String String Evergreen.V168.Parser.Token.Meta
    | Error String
