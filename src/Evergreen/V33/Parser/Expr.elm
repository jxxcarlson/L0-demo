module Evergreen.V33.Parser.Expr exposing (..)

import Evergreen.V33.Parser.Token


type Expr
    = Expr String (List Expr) Evergreen.V33.Parser.Token.Meta
    | Text String Evergreen.V33.Parser.Token.Meta
    | Verbatim String String Evergreen.V33.Parser.Token.Meta
    | Error String
