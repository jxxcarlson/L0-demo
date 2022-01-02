module Evergreen.V81.Parser.Expr exposing (..)

import Evergreen.V81.Parser.Token


type Expr
    = Expr String (List Expr) Evergreen.V81.Parser.Token.Meta
    | Text String Evergreen.V81.Parser.Token.Meta
    | Verbatim String String Evergreen.V81.Parser.Token.Meta
    | Error String
