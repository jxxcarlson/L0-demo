module Render.Block exposing (render)

import Dict exposing (Dict)
import Either exposing (Either(..))
import Element exposing (Element)
import Element.Events as Events
import Element.Font as Font
import List.Extra
import Parser.Block exposing (BlockType(..), L0BlockE(..))
import Parser.Expr exposing (Expr)
import Render.ASTTools as ASTTools
import Render.Elm
import Render.Math exposing (DisplayMode(..))
import Render.Msg exposing (MarkupMsg(..))
import Render.Settings exposing (Settings)
import Render.Utility


render : Int -> Settings -> L0BlockE -> Element MarkupMsg
render count settings (L0BlockE { name, args, indent, blockType, content, lineNumber, children }) =
    case blockType of
        Paragraph ->
            case content of
                Right exprs ->
                    List.map (Render.Elm.render count settings) exprs |> (\x -> Element.paragraph [ Events.onClick (SendLineNumber lineNumber) ] x)

                Left _ ->
                    Element.none

        VerbatimBlock _ ->
            case content of
                Right _ ->
                    Element.none

                Left str ->
                    case name of
                        Nothing ->
                            noSuchVerbatimBlock "name" str

                        Just functionName ->
                            case Dict.get functionName verbatimDict of
                                Nothing ->
                                    noSuchVerbatimBlock functionName str

                                Just f ->
                                    f count settings args lineNumber str

        OrdinaryBlock _ ->
            case content of
                Left _ ->
                    Element.none

                Right exprs ->
                    case name of
                        Nothing ->
                            noSuchOrdinaryBlock count settings "name" exprs

                        Just functionName ->
                            case Dict.get functionName blockDict of
                                Nothing ->
                                    noSuchOrdinaryBlock count settings functionName exprs

                                Just f ->
                                    f count settings args lineNumber exprs


noSuchVerbatimBlock : String -> String -> Element MarkupMsg
noSuchVerbatimBlock functionName content =
    Element.column [ Element.spacing 4 ]
        [ Element.paragraph [ Font.color (Element.rgb255 180 0 0) ] [ Element.text <| "|| " ++ functionName ++ " ?? " ]
        , Element.column [ Element.spacing 4 ] (List.map (\t -> Element.el [] (Element.text t)) (String.lines content))
        ]


noSuchOrdinaryBlock : Int -> Settings -> String -> List Expr -> Element MarkupMsg
noSuchOrdinaryBlock count settings functionName exprs =
    Element.column [ Element.spacing 4 ]
        [ Element.paragraph [ Font.color (Element.rgb255 180 0 0) ] [ Element.text <| "| " ++ functionName ++ " ?? " ]
        , Element.paragraph [] (List.map (Render.Elm.render count settings) exprs)
        ]


blockDict : Dict String (Int -> Settings -> List String -> Int -> List Expr -> Element MarkupMsg)
blockDict =
    Dict.fromList
        [ ( "indent", indented )
        , ( "heading", heading )
        , ( "title", \_ _ _ _ _ -> Element.none )
        , ( "subtitle", \_ _ _ _ _ -> Element.none )
        , ( "author", \_ _ _ _ _ -> Element.none )
        , ( "date", \_ _ _ _ _ -> Element.none )
        , ( "abstract", env "Abstract" )
        , ( "theorem", env "Theorem" )
        , ( "proposition", env "Proposition" )
        , ( "lemma", env "Lemma" )
        , ( "corollary", env "Corollary" )
        , ( "problem", env "Problem" )
        , ( "remark", env "Remark" )
        , ( "note", env "Note" )
        , ( "env", env_ )
        , ( "item", item )
        , ( "numbered", numbered )
        ]


verbatimDict : Dict String (Int -> Settings -> List String -> Int -> String -> Element MarkupMsg)
verbatimDict =
    Dict.fromList
        [ ( "math", renderDisplayMath )
        , ( "code", renderCode )
        ]


heading count settings args lineNumber exprs =
    -- level 1 is reserved for titles
    let
        headingLevel =
            case List.head args of
                Nothing ->
                    2

                Just level ->
                    String.toFloat level |> Maybe.withDefault 2 |> (\x -> x + 1)

        sectionNumber =
            List.Extra.getAt 1 args
                |> Maybe.withDefault ""
                |> (\s -> Element.el [ Font.size fontSize ] (Element.text (s ++ ". ")))

        fontSize =
            Render.Settings.maxHeadingFontSize / sqrt headingLevel |> round
    in
    Element.link
        [ Font.size fontSize
        , Render.Utility.makeId exprs
        , Render.Utility.elementAttribute "id" (String.fromInt lineNumber)
        , Events.onClick (SendLineNumber lineNumber)
        , Render.Utility.elementAttribute "id" (String.fromInt lineNumber)
        ]
        { url = Render.Utility.internalLink "TITLE", label = Element.paragraph [] (sectionNumber :: renderWithDefault "| heading" count settings exprs) }


verticalPadding top bottom =
    Element.paddingEach { top = top, bottom = bottom, left = 0, right = 0 }


renderWithDefault : String -> Int -> Settings -> List Expr -> List (Element MarkupMsg)
renderWithDefault default count settings exprs =
    if List.isEmpty exprs then
        [ Element.el [ Font.color Render.Settings.redColor, Font.size 14 ] (Element.text default) ]

    else
        List.map (Render.Elm.render count settings) exprs


indented count settings args lineNumber exprs =
    Element.paragraph [ Render.Settings.leftIndentation, Events.onClick (SendLineNumber lineNumber), Render.Utility.elementAttribute "id" (String.fromInt lineNumber) ]
        (renderWithDefault "| indent" count settings exprs)


env_ : Int -> Settings -> List String -> Int -> List Expr -> Element MarkupMsg
env_ count settings args lineNumber exprs =
    case List.head args of
        Nothing ->
            Element.paragraph [ Render.Utility.elementAttribute "id" (String.fromInt lineNumber), Font.color Render.Settings.redColor, Events.onClick (SendLineNumber lineNumber) ] [ Element.text "| env (missing name!)" ]

        Just name ->
            env name count settings (List.drop 1 args) lineNumber exprs


env : String -> Int -> Settings -> List String -> Int -> List Expr -> Element MarkupMsg
env name count settings args lineNumber exprs =
    let
        heading_ =
            if List.isEmpty args then
                name

            else
                name ++ " (" ++ String.join " " args ++ ")"
    in
    Element.column [ Element.spacing 8, Render.Utility.elementAttribute "id" (String.fromInt lineNumber) ]
        [ Element.el [ Font.bold, Events.onClick (SendLineNumber lineNumber) ] (Element.text heading_)
        , Element.paragraph [ Font.italic, Events.onClick (SendLineNumber lineNumber) ]
            (renderWithDefault ("| " ++ name) count settings exprs)
        ]


renderDisplayMath count settings args lineNumber str =
    let
        w =
            String.fromInt settings.width ++ "px"
    in
    Element.column [ Events.onClick (SendLineNumber lineNumber) ]
        [ Render.Math.mathText count w "id" DisplayMathMode str ]


renderCode count settings args lineNumber str =
    Element.column
        [ Font.color (Element.rgb255 170 0 250)
        , Font.family
            [ Font.typeface "Inconsolata"
            , Font.monospace
            ]
        , Element.spacing 8
        , Element.paddingEach { left = 24, right = 0, top = 0, bottom = 0 }
        , Events.onClick (SendLineNumber lineNumber)
        , Render.Utility.elementAttribute "id" (String.fromInt lineNumber)
        ]
        (List.map (\t -> Element.el [] (Element.text t)) (String.lines (String.trim str)))


removeFirstLine : String -> String
removeFirstLine str =
    str |> String.trim |> String.lines |> List.drop 1 |> String.join "\n"


item count settings args lineNumber exprs =
    Element.row [ Element.alignTop, Render.Utility.elementAttribute "id" (String.fromInt lineNumber) ]
        [ Element.el [ Font.size 18, Element.alignTop, Element.moveRight 6, Element.width (Element.px 24), Render.Settings.leftIndentation ] (Element.text "•")
        , Element.paragraph [ Render.Settings.leftIndentation, Events.onClick (SendLineNumber lineNumber) ]
            (renderWithDefault "| item" count settings exprs)
        ]


numbered count settings args lineNumber exprs =
    let
        label =
            List.Extra.getAt 0 args |> Maybe.withDefault ""
    in
    Element.row [ Element.alignTop, Render.Utility.elementAttribute "id" (String.fromInt lineNumber) ]
        [ Element.el
            [ Font.size 14
            , Element.alignTop
            , Element.moveRight 6
            , Element.width (Element.px 24)
            , Render.Settings.leftIndentation
            ]
            (Element.text (label ++ ". "))
        , Element.paragraph [ Render.Settings.leftIndentation, Events.onClick (SendLineNumber lineNumber) ]
            (renderWithDefault "| numbered" count settings exprs)
        ]
