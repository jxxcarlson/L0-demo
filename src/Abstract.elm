module Abstract exposing
    ( Abstract
    , AbstractOLD
    , empty
    , get
    , getItem
    , toString
    )

import Lang.Lang as Lang
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


get : Lang.Lang -> String -> Abstract
get lang source =
    let
        title =
            getItem lang "title" source

        author =
            getItem lang "author" source

        abstract =
            getItem lang "abstract" source

        tags =
            getItem lang "tags" source
    in
    { title = title
    , author = author
    , abstract = abstract
    , tags = tags
    , digest = [ title, author, abstract, tags ] |> String.join " " |> String.toLower
    }


getItem : Lang.Lang -> String -> String -> String
getItem lang itemName source =
    case Parser.run (itemParser lang itemName) source of
        Err _ ->
            ""

        Ok str ->
            str


itemParser : Lang.Lang -> String -> Parser String
itemParser lang name =
    case lang of
        Lang.Markdown ->
            annotationParser name

        Lang.MiniLaTeX ->
            macroParser name

        _ ->
            Parser.succeed ""


macroParser : String -> Parser String
macroParser name =
    let
        prefix =
            "\\" ++ name ++ "{"
    in
    Parser.succeed String.slice
        |. Parser.chompUntil prefix
        |. Parser.symbol prefix
        |= Parser.getOffset
        |. Parser.chompUntil "}"
        |= Parser.getOffset
        |= Parser.getSource


annotationParser : String -> Parser String
annotationParser name =
    Parser.succeed String.slice
        |. Parser.chompUntil "[!"
        |. Parser.chompUntil name
        |. Parser.chompUntil "]("
        |. Parser.symbol "]("
        |= Parser.getOffset
        |. Parser.chompUntil ")"
        |= Parser.getOffset
        |= Parser.getSource
