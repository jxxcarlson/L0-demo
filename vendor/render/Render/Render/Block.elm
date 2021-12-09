module Render.Block exposing (render)

import Block.Block exposing (BlockType(..), L0BlockE(..))
import Dict exposing (Dict)
import Either exposing (Either(..))
import Element exposing (Element)
import Element.Font as Font
import Parser.Expression exposing (Expr)
import Render.Elm
import Render.Math exposing (DisplayMode(..))
import Render.Msg exposing (MarkupMsg)
import Render.Settings exposing (Settings)


render : Int -> Settings -> L0BlockE -> Element MarkupMsg
render count settings (L0BlockE { name, args, indent, blockType, content, children }) =
    case blockType of
        Paragraph ->
            case content of
                Right exprs ->
                    List.map (Render.Elm.render count settings) exprs |> (\x -> Element.paragraph [] x)

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
                                    f count args str

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
                                    f count settings args exprs


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


blockDict : Dict String (Int -> Settings -> List String -> List Expr -> Element MarkupMsg)
blockDict =
    Dict.fromList
        [ ( "indent", indented )
        , ( "heading", heading )
        , ( "title", title )
        , ( "subtitle", subtitle )
        ]


verbatimDict : Dict String (Int -> List String -> String -> Element MarkupMsg)
verbatimDict =
    Dict.fromList
        [ ( "math", renderDisplayMath )
        , ( "code", renderCode )
        ]


title count settings args exprs =
    Element.paragraph [ Font.size (round Render.Settings.maxHeadingFontSize) ] (renderWithDefault "| heading" count settings exprs)


subtitle count settings args exprs =
    Element.paragraph
        [ Font.size (Render.Settings.maxHeadingFontSize / sqrt 3 |> round)

        --, Font.italic
        , Font.color (Element.rgb 0.4 0.4 0.4)
        ]
        (renderWithDefault "| heading" count settings exprs)


heading count settings args exprs =
    -- level 1 is reserved for titles
    let
        headingLevel =
            case List.head args of
                Nothing ->
                    2

                Just level ->
                    String.toFloat level |> Maybe.withDefault 2 |> (\x -> x + 1)

        fontSize =
            Render.Settings.maxHeadingFontSize / sqrt headingLevel |> round
    in
    Element.paragraph [ Font.size fontSize ] (renderWithDefault "| heading" count settings exprs)


renderWithDefault : String -> Int -> Settings -> List Expr -> List (Element MarkupMsg)
renderWithDefault default count settings exprs =
    if List.isEmpty exprs then
        [ Element.el [ Font.color Render.Settings.redColor, Font.size 14 ] (Element.text default) ]

    else
        List.map (Render.Elm.render count settings) exprs


indented count settings args exprs =
    Element.paragraph [ Render.Settings.leftIndentation ]
        (renderWithDefault "| indent" count settings exprs)


renderDisplayMath count args str =
    Render.Math.mathText count "id" DisplayMathMode str


renderCode count args str =
    Element.column
        [ Font.color (Element.rgb255 170 0 250)
        , Font.family
            [ Font.typeface "Inconsolata"
            , Font.monospace
            ]
        , Element.spacing 8
        , Element.paddingEach { left = 24, right = 0, top = 0, bottom = 0 }
        ]
        (List.map (\t -> Element.el [] (Element.text t)) (String.lines (String.trim str)))


removeFirstLine : String -> String
removeFirstLine str =
    str |> String.trim |> String.lines |> List.drop 1 |> String.join "\n"
