module Render.L0 exposing (renderFromAST, renderFromString, render_)

import Element exposing (Element)
import L0 exposing (AST)
import Render.Accumulator as Accumulator exposing (Accumulator)
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


render_ : AST -> List (Element MarkupMsg)
render_ ast =
    renderFromAST 0 Render.Settings.defaultSettings ast


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
            --  Render.Settings.leftIndentation,
            [ Tree.label tree
            , Element.column [ Element.paddingEach { top = Render.Settings.topMargin, left = Render.Settings.leftIndent, right = 0, bottom = 0 } ] (List.map unravel children)
            ]
