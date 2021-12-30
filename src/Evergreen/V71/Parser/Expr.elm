module Evergreen.V71.Parser.Expr exposing (..)

import Evergreen.V71.Parser.Token


type Expr
    = Expr String (List Expr) Evergreen.V71.Parser.Token.Meta
    | Text String Evergreen.V71.Parser.Token.Meta
    | Verbatim String String Evergreen.V71.Parser.Token.Meta
    | Error String
