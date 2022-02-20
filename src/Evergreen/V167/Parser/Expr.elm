module Evergreen.V167.Parser.Expr exposing (..)

import Evergreen.V167.Parser.Token


type Expr
    = Expr String (List Expr) Evergreen.V167.Parser.Token.Meta
    | Text String Evergreen.V167.Parser.Token.Meta
    | Verbatim String String Evergreen.V167.Parser.Token.Meta
    | Error String
