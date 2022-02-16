module Render.Elm exposing (render)

import Compiler.ASTTools as ASTTools
import Dict exposing (Dict)
import Element exposing (Element, alignLeft, alignRight, centerX, column, el, newTabLink, px, spacing)
import Element.Background as Background
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Parser.Expr exposing (Expr(..))
import Render.Math
import Render.Msg exposing (L0Msg(..))
import Render.Settings exposing (Settings)
import Render.Utility as Utility


render : Int -> Settings -> Expr -> Element L0Msg
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



--renderMarked : String -> Int -> Settings -> List Expr -> Element MarkupMsg
--renderMarked name generation settings exprList =
--    case Dict.get name markupDict of
--        Nothing ->
--            case Dict.get name settings.environment of
--                Nothing ->
--                    Element.paragraph [ spacing 8 ] (Element.el [ Font.color errorColor, Font.bold ] (Element.text name) :: List.map (render generation settings) exprList)
--
--                Just lambda ->
--                    let
--                        exprList2 =
--                            List.map (Lambda.apply lambda) exprList
--                    in
--                    List.map (render generation settings) exprList2
--
--        Just f ->
--            f generation settings exprList
-- DICTIONARIES


markupDict : Dict String (Int -> Settings -> List Expr -> Element L0Msg)
markupDict =
    Dict.fromList
        [ ( "bibitem", \g s exprList -> bibitem g s exprList )

        -- STYLE
        , ( "strong", \g s exprList -> strong g s exprList )
        , ( "bold", \g s exprList -> strong g s exprList )
        , ( "b", \g s exprList -> strong g s exprList )
        , ( "italic", \g s exprList -> italic g s exprList )
        , ( "i", \g s exprList -> italic g s exprList )
        , ( "boldItalic", \g s exprList -> boldItalic g s exprList )
        , ( "strike", \g s exprList -> strike g s exprList )
        , ( "underline", \g s exprList -> underline g s exprList )

        -- COLOR
        , ( "red", \g s exprList -> red g s exprList )
        , ( "blue", \g s exprList -> blue g s exprList )
        , ( "violet", \g s exprList -> violet g s exprList )
        , ( "highlight", \g s exprList -> highlight g s exprList )
        , ( "gray", \g s exprList -> gray g s exprList )
        , ( "errorHighlight", \g s exprList -> errorHighlight g s exprList )

        --
        , ( "skip", \g s exprList -> skip g s exprList )
        , ( "link", \g s exprList -> link g s exprList )
        , ( "ilink", \g s exprList -> ilink g s exprList )
        , ( "abstract", \g s exprList -> abstract g s exprList )
        , ( "large", \g s exprList -> large g s exprList )
        , ( "mdash", \g s exprList -> Element.el [] (Element.text "—") )
        , ( "ndash", \g s exprList -> Element.el [] (Element.text "–") )
        , ( "label", \g s exprList -> Element.none )
        , ( "cite", \g s exprList -> cite g s exprList )
        , ( "table", \g s exprList -> table g s exprList )
        , ( "image", \g s exprList -> image g s exprList )
        , ( "tags", invisible )
        , ( "vskip", vskip )

        -- MiniLaTeX stuff
        , ( "term", \g s exprList -> term g s exprList )
        , ( "emph", \g s exprList -> emph g s exprList )
        , ( "group", \g s exprList -> identityFunction g s exprList )

        --
        , ( "dollarSign", \_ _ _ -> Element.el [] (Element.text "$") )
        , ( "backTick", \_ _ _ -> Element.el [] (Element.text "`") )
        ]


verbatimDict =
    Dict.fromList
        [ ( "$", \g s m str -> math g s m str )
        , ( "`", \g s m str -> code g s m str )
        , ( "code", \g s m str -> code g s m str )
        , ( "math", \g s m str -> math g s m str )
        ]



-- FUNCTIONS


identityFunction g s exprList =
    Element.paragraph [] (List.map (render g s) exprList)


abstract g s exprList =
    Element.paragraph [] [ Element.el [ Font.size 18 ] (Element.text "Abstract."), simpleElement [] g s exprList ]


large : Int -> Settings -> List Expr -> Element L0Msg
large g s exprList =
    simpleElement [ Font.size 18 ] g s exprList


link : Int -> Settings -> List Expr -> Element L0Msg
link g s exprList =
    case List.head <| ASTTools.exprListToStringList exprList of
        Nothing ->
            errorText_ "Please provide label and url"

        Just argString ->
            let
                args =
                    String.words argString

                n =
                    List.length args

                label =
                    List.take (n - 1) args |> String.join " "

                url =
                    List.drop (n - 1) args |> String.join " "
            in
            newTabLink []
                { url = url
                , label = el [ Font.color linkColor ] (Element.text label)
                }


ilink g s exprList =
    case List.head <| ASTTools.exprListToStringList exprList of
        Nothing ->
            errorText_ "Please provide label and url"

        Just argString ->
            let
                args =
                    String.words argString

                n =
                    List.length args

                label =
                    List.take (n - 1) args |> String.join " "

                docId =
                    List.drop (n - 1) args |> String.join " "
            in
            Input.button []
                { onPress = Just (GetPublicDocument docId)
                , label = Element.el [ Element.centerX, Element.centerY, Font.size 14, Font.color (Element.rgb 0 0 0.8) ] (Element.text label)
                }


image generation settings body =
    let
        captionExpr =
            ASTTools.filterExpressionsOnName "caption" body |> List.head

        arguments : List String
        arguments =
            ASTTools.exprListToStringList body

        url =
            List.head arguments |> Maybe.withDefault "no-image"

        dict =
            Utility.keyValueDict (List.drop 1 arguments) |> Dict.insert "caption" (Maybe.andThen ASTTools.getText captionExpr |> Maybe.withDefault "")

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


bibitem : Int -> Settings -> List Expr -> Element L0Msg
bibitem generation settings str =
    Element.paragraph [ Element.width Element.fill ] [ Element.text (ASTTools.exprListToStringList str |> String.join " " |> (\s -> "[" ++ s ++ "]")) ]


cite : Int -> Settings -> List Expr -> Element L0Msg
cite generation settings str =
    Element.paragraph [ Element.width Element.fill ] [ Element.text (ASTTools.exprListToStringList str |> String.join " " |> (\s -> "[" ++ s ++ "]")) ]


code g s m str =
    verbatimElement codeStyle g s m str


math g s m str =
    mathElement g s m str


table : Int -> Settings -> List Expr -> Element L0Msg
table g s rows =
    Element.column [ Element.spacing 8 ] (List.map (tableRow g s) rows)


tableRow : Int -> Settings -> Expr -> Element L0Msg
tableRow g s expr =
    case expr of
        Expr "tableRow" items _ ->
            Element.row [ spacing 8 ] (List.map (tableItem g s) items)

        _ ->
            Element.none


tableItem : Int -> Settings -> Expr -> Element L0Msg
tableItem g s expr =
    case expr of
        Expr "tableItem" exprList _ ->
            Element.paragraph [ Element.width (Element.px 100) ] (List.map (render g s) exprList)

        _ ->
            Element.none


verticalPadding top bottom =
    Element.paddingEach { top = top, bottom = bottom, left = 0, right = 0 }


skip g s exprList =
    let
        numVal : String -> Int
        numVal str =
            String.toInt str |> Maybe.withDefault 0

        f : String -> Element L0Msg
        f str =
            column [ Element.spacingXY 0 (numVal str) ] [ Element.text "" ]
    in
    f1 f g s exprList


vskip g s exprList =
    let
        h =
            ASTTools.exprListToStringList exprList |> String.join "" |> String.toInt |> Maybe.withDefault 0
    in
    -- Element.column [ Element.paddingXY 0 100 ] (Element.text "-")
    Element.column [ Element.height (Element.px h) ] [ Element.text "" ]


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



-- COLOR FUNCTIONS


gray g s exprList =
    simpleElement [ Font.color (Element.rgb 0.5 0.5 0.5) ] g s exprList


red g s exprList =
    simpleElement [ Font.color (Element.rgb255 200 0 0) ] g s exprList


blue g s exprList =
    simpleElement [ Font.color (Element.rgb255 0 0 200) ] g s exprList


violet g s exprList =
    simpleElement [ Font.color (Element.rgb255 150 100 255) ] g s exprList


highlight g s exprList =
    simpleElement [ Background.color (Element.rgb255 255 255 0) ] g s exprList



-- FONT STYLE FUNCTIONS


strike g s exprList =
    simpleElement [ Font.strike ] g s exprList


underline g s exprList =
    simpleElement [ Font.underline ] g s exprList


errorHighlight g s exprList =
    simpleElement [ Background.color (Element.rgb255 255 200 200), Element.paddingXY 2 2 ] g s exprList



-- HELPERS


getArgs : List Expr -> List String
getArgs exprs =
    exprs
        |> ASTTools.exprListToStringList
        |> String.join " "
        |> String.words


simpleElement : List (Element.Attribute L0Msg) -> Int -> Settings -> List Expr -> Element L0Msg
simpleElement formatList g s exprList =
    Element.paragraph formatList (List.map (render g s) exprList)


{-| For one-element functions
-}
f1 : (String -> Element L0Msg) -> Int -> Settings -> List Expr -> Element L0Msg
f1 f g s exprList =
    case ASTTools.exprListToStringList exprList of
        -- TODO: temporary fix: parse is producing the args in reverse order
        arg1 :: _ ->
            f arg1

        _ ->
            el [ Font.color errorColor ] (Element.text "Invalid arguments")


{-| For two-element functions
-}
f2 : (String -> String -> Element L0Msg) -> Int -> Settings -> List Expr -> Element L0Msg
f2 element g s exprList =
    case ASTTools.exprListToStringList exprList of
        -- TODO: temporary fix: parse is producing the args in reverse order
        arg1 :: arg2 :: _ ->
            element arg1 arg2

        _ ->
            el [ Font.color errorColor ] (Element.text "Invalid arguments")


verbatimElement formatList g s m str =
    Element.el (htmlId "DUMMY_ID" :: formatList) (Element.text str)


htmlId str =
    Element.htmlAttribute (Html.Attributes.id str)


errorText index str =
    Element.el [ Font.color (Element.rgb255 200 40 40) ] (Element.text <| "(" ++ String.fromInt index ++ ") not implemented: " ++ str)


errorText_ str =
    Element.el [ Font.color (Element.rgb255 200 40 40) ] (Element.text str)


invisible g s exprList =
    Element.none


mathElement generation settings m str =
    -- "width" is not used for inline math, but some string needs to be there
    Render.Math.mathText generation "width" "DUMMY_ID" Render.Math.InlineMathMode str


makeSlug : String -> String
makeSlug str =
    str |> String.toLower |> String.replace " " "-"


makeId : List Expr -> Element.Attribute L0Msg
makeId exprList =
    Utility.elementAttribute "id" (ASTTools.stringValueOfList exprList |> String.trim |> makeSlug)


internalLink : String -> String
internalLink str =
    "#" ++ str |> makeSlug



-- DEFINITIONS


blueColor =
    Element.rgb 0 0 0.8


codeColor =
    -- E.rgb 0.2 0.5 1.0
    Element.rgb 0.4 0 0.8


codeStyle =
    [ Font.family
        [ Font.typeface "Inconsolata"
        , Font.monospace
        ]
    , Font.unitalicized
    , Font.color codeColor
    , Element.paddingEach { left = 2, right = 2, top = 0, bottom = 0 }
    ]


errorColor =
    Element.rgb 0.8 0 0


linkColor =
    Element.rgb 0 0 0.8


redColor =
    Element.rgb 0.6 0 0.8
