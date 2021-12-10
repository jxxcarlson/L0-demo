module Render.Utility exposing
    ( elementAttribute
    , internalLink
    , keyValueDict
    , makeId
    )

import Dict exposing (Dict)
import Element
import Html.Attributes
import Maybe.Extra
import Parser.Expr
import Render.ASTTools


internalLink : String -> String
internalLink str =
    "#" ++ str |> makeSlug


makeId : List Parser.Expr.Expr -> Element.Attribute msg
makeId exprs =
    elementAttribute "id"
        (Render.ASTTools.stringValueOfList exprs |> String.trim |> makeSlug)


makeSlug : String -> String
makeSlug str =
    str |> String.toLower |> String.replace " " ""


keyValueDict : List String -> Dict String String
keyValueDict strings_ =
    List.map (String.split ":") strings_
        |> List.map (List.map String.trim)
        |> List.map pairFromList
        |> Maybe.Extra.values
        |> Dict.fromList


pairFromList : List String -> Maybe ( String, String )
pairFromList strings =
    case strings of
        [ x, y ] ->
            Just ( x, y )

        _ ->
            Nothing


elementAttribute : String -> String -> Element.Attribute msg
elementAttribute key value =
    Element.htmlAttribute (Html.Attributes.attribute key value)
