module Render.TOC exposing (view)

import Either exposing (Either(..))
import Element exposing (Element)
import Element.Font as Font
import L0
import List.Extra
import Parser.Block exposing (BlockType(..), ExpressionBlock(..))
import Parser.Expr exposing (Expr)
import Render.ASTTools
import Render.Elm
import Render.Msg exposing (L0Msg(..))
import Render.Settings
import Render.Utility
import Tree


view : Int -> Render.Settings.Settings -> L0.SyntaxTree -> Element Render.Msg.L0Msg
view counter settings ast =
    case ast |> List.map Tree.flatten |> List.concat |> Render.ASTTools.filterBlocksOnName "makeTableOfContents" of
        [] ->
            Element.column [ Element.spacing 8, Element.paddingEach { left = 0, right = 0, top = 0, bottom = 36 } ]
                (prepareFrontMatter counter Render.Settings.defaultSettings ast)

        _ ->
            Element.column [ Element.spacing 8, Element.paddingEach { left = 0, right = 0, top = 0, bottom = 36 } ]
                (prepareTOC counter Render.Settings.defaultSettings ast)


viewTocItem : Int -> Render.Settings.Settings -> ExpressionBlock -> Element Render.Msg.L0Msg
viewTocItem count settings (ExpressionBlock { args, content, lineNumber }) =
    case content of
        Left _ ->
            Element.none

        Right exprs ->
            let
                t =
                    String.fromInt lineNumber

                sectionNumber =
                    List.Extra.getAt 1 args
                        |> Maybe.withDefault ""
                        |> (\s -> Element.el [] (Element.text (s ++ ". ")))

                label : Element L0Msg
                label =
                    Element.paragraph [ tocIndent args ] (sectionNumber :: List.map (Render.Elm.render count settings) exprs)
            in
            Element.link [ Font.color (Element.rgb 0 0 0.8) ] { url = Render.Utility.internalLink t, label = label }


prepareTOC : Int -> Render.Settings.Settings -> L0.SyntaxTree -> List (Element L0Msg)
prepareTOC count settings ast =
    let
        rawToc =
            Render.ASTTools.tableOfContents ast

        toc =
            Element.el [ Font.bold, Font.size 18 ] (Element.text "Contents")
                :: (rawToc |> List.map (viewTocItem count settings))

        headings =
            getHeadings ast

        titleSize =
            Font.size (round Render.Settings.maxHeadingFontSize)

        subtitleSize =
            Font.size (round (0.7 * Render.Settings.maxHeadingFontSize))

        idAttr =
            Render.Utility.elementAttribute "id" "title"

        title =
            headings.title
                |> Maybe.map (List.map (Render.Elm.render count settings) >> Element.paragraph [ titleSize, idAttr ])
                |> Maybe.withDefault Element.none

        subtitle =
            headings.subtitle
                |> Maybe.map (List.map (Render.Elm.render count settings) >> Element.paragraph [ subtitleSize, Font.color (Element.rgb 0.4 0.4 0.4) ])
                |> Maybe.withDefault Element.none

        spaceBelow k =
            Element.el [ Element.paddingEach { bottom = k, top = 0, left = 0, right = 0 } ] (Element.text " ")
    in
    if List.length rawToc < 2 then
        title :: subtitle :: []

    else
        title :: subtitle :: spaceBelow 8 :: toc


prepareFrontMatter : Int -> Render.Settings.Settings -> L0.SyntaxTree -> List (Element L0Msg)
prepareFrontMatter count settings ast =
    let
        headings =
            getHeadings ast

        titleSize =
            Font.size (round Render.Settings.maxHeadingFontSize)

        subtitleSize =
            Font.size (round (0.7 * Render.Settings.maxHeadingFontSize))

        idAttr =
            Render.Utility.elementAttribute "id" "title"

        title =
            headings.title
                |> Maybe.map (List.map (Render.Elm.render count settings) >> Element.paragraph [ titleSize, idAttr ])
                |> Maybe.withDefault Element.none

        subtitle =
            headings.subtitle
                |> Maybe.map (List.map (Render.Elm.render count settings) >> Element.paragraph [ subtitleSize, Font.color (Element.rgb 0.4 0.4 0.4) ])
                |> Maybe.withDefault Element.none

        spaceBelow k =
            Element.el [ Element.paddingEach { bottom = k, top = 0, left = 0, right = 0 } ] (Element.text " ")
    in
    title :: subtitle :: []


tocLink : String -> List Expr -> Element L0Msg
tocLink label exprList =
    let
        t =
            Render.ASTTools.stringValueOfList exprList
    in
    Element.link [] { url = Render.Utility.internalLink t, label = Element.text (label ++ " " ++ t) }


tocIndent args =
    Element.paddingEach { left = tocIndentAux args, right = 0, top = 0, bottom = 0 }


tocIndentAux args =
    case List.head args of
        Nothing ->
            0

        Just str ->
            String.toInt str |> Maybe.withDefault 0 |> (\x -> 12 * x)


getHeadings : L0.SyntaxTree -> { title : Maybe (List Expr), subtitle : Maybe (List Expr) }
getHeadings ast =
    let
        data =
            ast |> Render.ASTTools.title |> Render.ASTTools.toExprRecord

        title : Maybe (List Expr)
        title =
            data
                |> List.filter (\item -> item.blockType == OrdinaryBlock [ "title" ])
                |> List.head
                |> Maybe.map .content

        subtitle =
            data
                |> List.filter (\item -> item.blockType == OrdinaryBlock [ "subtitle" ])
                |> List.head
                |> Maybe.map .content
    in
    { title = title, subtitle = subtitle }
