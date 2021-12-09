module Render.L0 exposing (renderFromAST, renderFromString)

import Element exposing (Element)
import L0 exposing (AST)
import Render.Block
import Render.Msg exposing (MarkupMsg)
import Render.Settings exposing (Settings)
import Tree exposing (Tree)


isVerbatimLine : String -> Bool
isVerbatimLine str =
    String.left 2 str == "||"


renderFromString : Int -> Settings -> String -> List (Element MarkupMsg)
renderFromString count settings str =
    str |> L0.parse |> renderFromAST count settings


renderFromAST : Int -> Settings -> AST -> List (Element MarkupMsg)
renderFromAST count settings ast =
    ast
        |> List.map (Tree.map (Render.Block.render count settings))
        |> List.map unravel


unravel : Tree (Element MarkupMsg) -> Element MarkupMsg
unravel tree =
    let
        children =
            Tree.children tree
    in
    if List.isEmpty children then
        Tree.label tree

    else
        Element.column []
            [ Tree.label tree
            , Element.column [ Render.Settings.leftIndentation ] (List.map unravel children)
            ]
