module Evergreen.V48.Parser.Expr exposing (..)

import Evergreen.V48.Parser.Token


type Expr
    = Expr String (List Expr) Evergreen.V48.Parser.Token.Meta
    | Text String Evergreen.V48.Parser.Token.Meta
    | Verbatim String String Evergreen.V48.Parser.Token.Meta
    | Error String
