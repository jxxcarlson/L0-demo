module Render.Elm exposing (render)

import ASTTools
import Dict exposing (Dict)
import Element exposing (Element, alignLeft, alignRight, centerX, column, el, newTabLink, px, spacing)
import Element.Background as Background
import Element.Events as Events
import Element.Font as Font
import Html.Attributes
import Parser.Expression exposing (Expr(..))
import Render.Math
import Render.Msg exposing (MarkupMsg(..))
import Render.Settings exposing (Settings)
import Utility


render : Int -> Settings -> Expr -> Element MarkupMsg
render generation settings expr =
    case expr of
        Text string meta ->
            Element.el [ Events.onClick (SendMeta meta), htmlId "DUMMY_ID" ] (Element.text string)

        Expr name exprList meta ->
            Element.el [ htmlId "DUMMY_ID" ] (renderMarked name generation settings exprList)

        Verbatim name str meta ->
            renderVerbatim name generation settings meta str

        Error str ->
            Element.el [ Font.color redColor ] (Element.text str)


htmlId str =
    Element.htmlAttribute (Html.Attributes.id str)


errorText index str =
    Element.el [ Font.color (Element.rgb255 200 40 40) ] (Element.text <| "(" ++ String.fromInt index ++ ") not implemented: " ++ str)


renderVerbatim name generation settings meta str =
    case Dict.get name verbatimDict of
        Nothing ->
            errorText 1 name

        Just f ->
            f generation settings meta str


renderMarked name generation settings exprList =
    case Dict.get name markupDict of
        Nothing ->
            Element.paragraph [ spacing 8 ] (Element.el [ Font.color errorColor, Font.bold ] (Element.text name) :: List.map (render generation settings) exprList)

        Just f ->
            f generation settings exprList


markupDict : Dict String (Int -> Settings -> List Expr -> Element MarkupMsg)
markupDict =
    Dict.fromList
        [ ( "item", \g s exprList -> item g s exprList )
        , ( "bibitem", \g s exprList -> bibitem g s exprList )
        , ( "numberedItem", \g s exprList -> numberedItem g s exprList )
        , ( "strong", \g s exprList -> strong g s exprList )
        , ( "bold", \g s exprList -> strong g s exprList )
        , ( "b", \g s exprList -> strong g s exprList )
        , ( "italic", \g s exprList -> italic g s exprList )
        , ( "i", \g s exprList -> italic g s exprList )
        , ( "boldItalic", \g s exprList -> boldItalic g s exprList )
        , ( "red", \g s exprList -> red g s exprList )
        , ( "blue", \g s exprList -> blue g s exprList )
        , ( "violet", \g s exprList -> violet g s exprList )
        , ( "highlight", \g s exprList -> highlight g s exprList )
        , ( "strike", \g s exprList -> strike g s exprList )
        , ( "underline", \g s exprList -> underline g s exprList )
        , ( "gray", \g s exprList -> gray g s exprList )
        , ( "errorHighlight", \g s exprList -> errorHighlight g s exprList )
        , ( "heading5", \g s exprList -> italic g s exprList )
        , ( "skip", \g s exprList -> skip g s exprList )
        , ( "link", \g s exprList -> link g s exprList )
        , ( "href", \g s exprList -> href g s exprList )
        , ( "abstract", \g s exprList -> abstract g s exprList )
        , ( "large", \g s exprList -> large g s exprList )
        , ( "mdash", \g s exprList -> Element.el [] (Element.text "—") )
        , ( "ndash", \g s exprList -> Element.el [] (Element.text "–") )
        , ( "label", \g s exprList -> Element.none )
        , ( "cite", \g s exprList -> cite g s exprList )
        , ( "table", \g s exprList -> table g s exprList )
        , ( "image", \g s exprList -> image g s exprList )

        -- MiniLaTeX stuff
        , ( "term", \g s exprList -> term g s exprList )
        , ( "emph", \g s exprList -> emph g s exprList )
        ]


verbatimDict =
    Dict.fromList
        [ ( "$", \g s m str -> math g s m str )
        , ( "`", \g s m str -> code g s m str )
        , ( "code", \g s m str -> code g s m str )
        , ( "math", \g s m str -> math g s m str )
        ]


redColor =
    Element.rgb 0.6 0 0.8


blueColor =
    Element.rgb 0 0 0.8


large g s exprList =
    simpleElement [ Font.size 18 ] g s exprList


abstract g s exprList =
    Element.paragraph [] [ Element.el [ Font.size 18 ] (Element.text "Abstract."), simpleElement [] g s exprList ]



--link g s exprList =
--    case exprList of
--        (Text label _) :: (Text url _) :: _ ->
--            link_ url label
--
--        _ ->
--            el [ Font.color errorColor ] (Element.text "bad data for link")


link g s exprList =
    let
        args =
            exprList |> ASTTools.exprListToStringList |> List.filter (\t -> String.trim t /= "")

        n =
            List.length args

        label =
            List.take (n - 1) args |> String.join " "

        url =
            List.drop (n - 1) args |> String.join ""
    in
    link_ url label


link_ : String -> String -> Element MarkupMsg
link_ url label =
    newTabLink []
        { url = url
        , label = el [ Font.color linkColor ] (Element.text label)
        }


macro1 : (String -> Element MarkupMsg) -> Int -> Settings -> List Expr -> Element MarkupMsg
macro1 f g s exprList =
    case ASTTools.exprListToStringList exprList of
        -- TODO: temporary fix: parse is producing the args in reverse order
        arg1 :: _ ->
            f arg1

        _ ->
            el [ Font.color errorColor ] (Element.text "Invalid arguments")


macro2 : (String -> String -> Element MarkupMsg) -> Int -> Settings -> List Expr -> Element MarkupMsg
macro2 element g s exprList =
    case ASTTools.exprListToStringList exprList of
        -- TODO: temporary fix: parse is producing the args in reverse order
        arg1 :: arg2 :: _ ->
            element arg1 arg2

        _ ->
            el [ Font.color errorColor ] (Element.text "Invalid arguments")


href g s exprList =
    macro2 href_ g s exprList


href_ : String -> String -> Element MarkupMsg
href_ url label =
    newTabLink []
        { url = url
        , label = el [ Font.color linkColor, Font.italic ] (Element.text <| label)
        }



--         , ( "href", \g s exprList -> href g s exprList )


image generation settings body =
    let
        arguments : List String
        arguments =
            ASTTools.exprListToStringList body

        url =
            List.head arguments |> Maybe.withDefault "no-image"

        dict =
            Utility.keyValueDict (List.drop 1 arguments)

        description =
            Dict.get "caption" dict |> Maybe.withDefault ""

        caption =
            case Dict.get "caption" dict of
                Nothing ->
                    Element.none

                Just c ->
                    Element.row [ placement, Element.width Element.fill ] [ el [ Element.width Element.fill ] (Element.text c) ]

        width =
            case Dict.get "width" dict of
                Nothing ->
                    px displayWidth

                Just w_ ->
                    case String.toInt w_ of
                        Nothing ->
                            px displayWidth

                        Just w ->
                            px w

        placement =
            case Dict.get "placement" dict of
                Nothing ->
                    centerX

                Just "left" ->
                    alignLeft

                Just "right" ->
                    alignRight

                Just "center" ->
                    centerX

                _ ->
                    centerX

        displayWidth =
            settings.width
    in
    column [ spacing 8, Element.width (px settings.width), placement, Element.paddingXY 0 18 ]
        [ Element.image [ Element.width width, placement ]
            { src = url, description = description }
        , el [ placement ] caption
        ]


errorColor =
    Element.rgb 0.8 0 0


linkColor =
    Element.rgb 0 0 0.8


simpleElement : List (Element.Attribute MarkupMsg) -> Int -> Settings -> List Expr -> Element MarkupMsg
simpleElement formatList g s exprList =
    Element.paragraph formatList (List.map (render g s) exprList)


verbatimElement formatList g s m str =
    Element.el (htmlId "DUMMY_ID" :: formatList) (Element.text str)


code g s m str =
    verbatimElement codeStyle g s m str


math g s m str =
    mathElement g s m str


codeStyle =
    [ Font.family
        [ Font.typeface "Inconsolata"
        , Font.monospace
        ]
    , Font.color codeColor
    , Element.paddingEach { left = 2, right = 2, top = 0, bottom = 0 }
    ]


mathElement generation settings m str =
    Render.Math.mathText generation "DUMMY_ID" Render.Math.InlineMathMode str


item : Int -> Settings -> List Expr -> Element MarkupMsg
item generation settings str =
    Element.paragraph [ Element.width Element.fill ] [ Element.text (ASTTools.exprListToStringList str |> String.join " ") ]


bibitem : Int -> Settings -> List Expr -> Element MarkupMsg
bibitem generation settings str =
    Element.paragraph [ Element.width Element.fill ] [ Element.text (ASTTools.exprListToStringList str |> String.join " " |> (\s -> "[" ++ s ++ "]")) ]


cite : Int -> Settings -> List Expr -> Element MarkupMsg
cite generation settings str =
    Element.paragraph [ Element.width Element.fill ] [ Element.text (ASTTools.exprListToStringList str |> String.join " " |> (\s -> "[" ++ s ++ "]")) ]


numberedItem : Int -> Settings -> List Expr -> Element MarkupMsg
numberedItem generation settings str =
    Element.paragraph [ Element.width Element.fill ] [ Element.text (ASTTools.exprListToStringList str |> String.join " ") ]


table : Int -> Settings -> List Expr -> Element MarkupMsg
table g s rows =
    Element.column [ Element.spacing 8 ] (List.map (tableRow g s) rows)


tableRow : Int -> Settings -> Expr -> Element MarkupMsg
tableRow g s expr =
    case expr of
        Expr "tableRow" items _ ->
            Element.row [ spacing 8 ] (List.map (tableItem g s) items)

        _ ->
            Element.none


tableItem : Int -> Settings -> Expr -> Element MarkupMsg
tableItem g s expr =
    case expr of
        Expr "tableItem" exprList _ ->
            Element.paragraph [ Element.width (Element.px 100) ] (List.map (render g s) exprList)

        _ ->
            Element.none


codeColor =
    -- E.rgb 0.2 0.5 1.0
    Element.rgb 0.4 0 0.8


internalLink : String -> String
internalLink str =
    "#" ++ str |> makeSlug


makeSlug : String -> String
makeSlug str =
    str |> String.toLower |> String.replace " " "-"


makeId : List Expr -> Element.Attribute MarkupMsg
makeId exprList =
    Utility.elementAttribute "id" (ASTTools.stringValueOfList exprList |> String.trim |> makeSlug)


verticalPadding top bottom =
    Element.paddingEach { top = top, bottom = bottom, left = 0, right = 0 }


skip g s exprList =
    let
        numVal : String -> Int
        numVal str =
            String.toInt str |> Maybe.withDefault 0

        f : String -> Element MarkupMsg
        f str =
            column [ Element.spacingXY 0 (numVal str) ] [ Element.text "" ]
    in
    macro1 f g s exprList


strong g s exprList =
    simpleElement [ Font.bold ] g s exprList


italic g s exprList =
    simpleElement [ Font.italic, Element.paddingEach { left = 0, right = 2, top = 0, bottom = 0 } ] g s exprList


boldItalic g s exprList =
    simpleElement [ Font.italic, Font.bold, Element.paddingEach { left = 0, right = 2, top = 0, bottom = 0 } ] g s exprList


term g s exprList =
    simpleElement [ Font.italic, Element.paddingEach { left = 0, right = 2, top = 0, bottom = 0 } ] g s exprList


emph g s exprList =
    simpleElement [ Font.italic, Element.paddingEach { left = 0, right = 2, top = 0, bottom = 0 } ] g s exprList


red g s exprList =
    simpleElement [ Font.color (Element.rgb255 200 0 0) ] g s exprList


blue g s exprList =
    simpleElement [ Font.color (Element.rgb255 0 0 200) ] g s exprList


violet g s exprList =
    simpleElement [ Font.color (Element.rgb255 150 100 255) ] g s exprList


highlight g s exprList =
    simpleElement [ Background.color (Element.rgb255 255 255 0) ] g s exprList


strike g s exprList =
    simpleElement [ Font.strike ] g s exprList


underline g s exprList =
    simpleElement [ Font.underline ] g s exprList


gray g s exprList =
    simpleElement [ Font.color (Element.rgb 0.5 0.5 0.5) ] g s exprList


errorHighlight g s exprList =
    simpleElement [ Background.color (Element.rgb255 255 200 200), Element.paddingXY 2 2 ] g s exprList
