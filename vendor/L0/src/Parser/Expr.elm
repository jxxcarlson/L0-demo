module Parser.Expr exposing (Expr(..))

{-| The syntax tree for the parser is of type Expr. In the examples below, we use `Parser.Expression.parse`;
in `parse 0 STRING` 0 is the line number at which the text begins. The Meta component
of an expression gives the position of the text of the expression in the source text and
other information, e.g., the index of the associated token in the list of tokens from
which it is derived.

    - Variant Expr:

    > parse 0 "[italic stuff]"
      [Expr "italic"
        [Text (" stuff") { begin = 7, end = 12, index = 2 }]
        { begin = 1, end = 6, index = 1 }
      ]

    - Variant Text:

    > parse 0 "this is a test"
      [Text ("this is a test") { begin = 0, end = 13, index = 0 }]

    - Variant Verbatim:

    > parse 0 "$x^2 = y^3$"
      [Verbatim "math" ("x^2 = y^3") { begin = 0, end = 0, index = 0 }]

    > parse 0 "`a[0] := 17;`"
      [Verbatim "code" ("a[0] := 17;") { begin = 0, end = 0, index = 0 }]

@docs Expr

-}

import Parser.Token exposing (Meta)


{-| -}
type Expr
    = Expr String (List Expr) Meta
    | Text String Meta
    | Verbatim String String Meta
    | Error String
