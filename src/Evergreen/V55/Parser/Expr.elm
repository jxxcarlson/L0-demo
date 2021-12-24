module Evergreen.V55.Parser.Expr exposing (..)

import Evergreen.V55.Parser.Token


type Expr
    = Expr String (List Expr) Evergreen.V55.Parser.Token.Meta
    | Text String Evergreen.V55.Parser.Token.Meta
    | Verbatim String String Evergreen.V55.Parser.Token.Meta
    | Error String
