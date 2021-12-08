module Abstract exposing
    ( Abstract
    , AbstractOLD
    , empty
    , get
    , getItem
    , toString
    )

import Parser exposing ((|.), (|=), Parser)


type alias Abstract =
    { title : String, author : String, abstract : String, tags : String, digest : String }


type alias AbstractOLD =
    { title : String, author : String, abstract : String, tags : String }


toString : Abstract -> String
toString a =
    [ a.title, a.author, a.tags ] |> String.join "; "


empty =
    { title = ""
    , author = ""
    , abstract = ""
    , tags = ""
    , digest = ""
    }


get : String -> Abstract
get source =
    let
        title =
            getItem "title" source

        author =
            getItem "author" source

        abstract =
            getItem "abstract" source

        tags =
            getItem "tags" source
    in
    { title = title
    , author = author
    , abstract = abstract
    , tags = tags
    , digest = [ title, author, abstract, tags ] |> String.join " " |> String.toLower
    }


getItem : String -> String -> String
getItem itemName source =
    case Parser.run (itemParser itemName) source of
        Err _ ->
            "??"

        Ok str ->
            str


{-|

    > getItem "title" "o [foo bar] ho ho ho [title Foo] blah blah"
    "Foo" : String

-}
itemParser : String -> Parser String
itemParser name =
    Parser.succeed String.slice
        |. Parser.chompUntil "["
        |. Parser.chompUntil name
        |. Parser.symbol name
        |. Parser.spaces
        |= Parser.getOffset
        |. Parser.chompUntil "]"
        |= Parser.getOffset
        |= Parser.getSource
