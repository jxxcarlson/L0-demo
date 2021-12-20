module Render.L0 exposing (renderFromAST, renderFromString, render_)

import Element exposing (Element)
import L0 exposing (SyntaxTree)
import Render.Acc as Accumulator exposing (Accumulator)
import Render.Block
import Render.Msg exposing (L0Msg)
import Render.Settings exposing (Settings)
import Tree exposing (Tree)


isVerbatimLine : String -> Bool
isVerbatimLine str =
    String.left 2 str == "||"


renderFromString : Int -> Settings -> String -> List (Element L0Msg)
renderFromString count settings str =
    str |> L0.parse |> renderFromAST count settings


render_ : SyntaxTree -> List (Element L0Msg)
render_ ast =
    renderFromAST 0 Render.Settings.defaultSettings ast


renderFromAST : Int -> Settings -> SyntaxTree -> List (Element L0Msg)
renderFromAST count settings ast =
    ast
        |> List.map (Tree.map (Render.Block.render count settings))
        |> List.map unravel


{-| Comment on this!
-}
unravel : Tree (Element L0Msg) -> Element L0Msg
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
            , Element.column [ Element.paddingEach { top = Render.Settings.topMarginForChildren, left = Render.Settings.leftIndent, right = 0, bottom = 0 } ] (List.map unravel children)
            ]
