module Render.TOC exposing (view)

import Either exposing (Either(..))
import Element exposing (Element)
import Element.Font as Font
import L0
import Parser.Block exposing (BlockType(..), L0BlockE(..))
import Parser.Expr exposing (Expr)
import Render.ASTTools
import Render.Elm
import Render.Msg exposing (MarkupMsg(..))
import Render.Settings
import Render.Utility


view : Int -> L0.AST -> Element Render.Msg.MarkupMsg
view counter ast =
    Element.column [ Element.spacing 8, Element.paddingEach { left = 0, right = 0, top = 0, bottom = 36 } ]
        (prepareTOC counter Render.Settings.defaultSettings ast)


viewTocItem : Int -> Render.Settings.Settings -> L0BlockE -> Element Render.Msg.MarkupMsg
viewTocItem count settings (L0BlockE { args, content }) =
    case content of
        Left _ ->
            Element.none

        Right exprs ->
            let
                t =
                    Render.ASTTools.stringValueOfList exprs

                label : Element MarkupMsg
                label =
                    Element.paragraph [ tocIndent args ] (List.map (Render.Elm.render count settings) exprs)
            in
            -- Element.paragraph [ tocIndent args ] (List.map (Render.Elm.render count settings) exprs)
            Element.link [ Font.color (Element.rgb 0 0 0.8) ] { url = Render.Utility.internalLink t, label = label }


prepareTOC : Int -> Render.Settings.Settings -> L0.AST -> List (Element MarkupMsg)
prepareTOC count settings ast =
    let
        toc =
            Element.el [ Font.bold, Font.size 18 ] (Element.text "Contents")
                :: (Render.ASTTools.tableOfContents ast |> List.map (viewTocItem count settings))

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
    if List.isEmpty toc then
        title :: subtitle :: []

    else
        title :: subtitle :: spaceBelow 8 :: toc


tocLink : String -> List Expr -> Element MarkupMsg
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


getHeadings : L0.AST -> { title : Maybe (List Expr), subtitle : Maybe (List Expr) }
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
