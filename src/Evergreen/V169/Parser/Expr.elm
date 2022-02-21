module Evergreen.V169.Parser.Expr exposing (..)

import Evergreen.V169.Parser.Token


type Expr
    = Expr String (List Expr) Evergreen.V169.Parser.Token.Meta
    | Text String Evergreen.V169.Parser.Token.Meta
    | Verbatim String String Evergreen.V169.Parser.Token.Meta
    | Error String
