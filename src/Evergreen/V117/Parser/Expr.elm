module Evergreen.V117.Parser.Expr exposing (..)

import Evergreen.V117.Parser.Token


type Expr
    = Expr String (List Expr) Evergreen.V117.Parser.Token.Meta
    | Text String Evergreen.V117.Parser.Token.Meta
    | Verbatim String String Evergreen.V117.Parser.Token.Meta
    | Error String
