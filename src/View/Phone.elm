module View.Phone exposing (view)

import Config
import Document exposing (Access(..), Document)
import Either exposing (Either(..))
import Element as E exposing (Element)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as HtmlAttr exposing (attribute)
import Html.Events
import Json.Decode
import L0
import Parser.Block exposing (ExpressionBlock(..))
import Parser.Expr exposing (Expr)
import Render.Elm
import Render.L0
import Render.Msg
import Render.Settings
import Render.TOC
import String.Extra
import Types exposing (..)
import View.Button as Button
import View.Color as Color
import View.Input
import View.Style
import View.Utility exposing (hideIf, showIf)


type alias Model =
    FrontendModel


view : Model -> Html FrontendMsg
view model =
    E.layoutWith { options = [ E.focusStyle View.Utility.noFocus ] }
        [ View.Style.bgGray 0.9, E.clipX, E.clipY ]
        (case model.phoneMode of
            PMShowDocument ->
                E.column []
                    [ E.row [ E.height (E.px 40), E.paddingXY 12 2, Font.size 14 ] [ Button.showTOCInPhone ]
                    , viewRendered model (smallPanelWidth model.windowWidth)
                    ]

            PMShowDocumentList ->
                E.column []
                    [ header model (E.px <| smallPanelWidth model.windowWidth)
                    , E.column
                        [ E.paddingEach { left = 0, right = 0, top = 0, bottom = 20 }
                        , View.Style.bgGray 1.0
                        , E.width (E.px <| smallPanelWidth model.windowWidth)
                        , E.height (E.px (appHeight_ model))
                        , Font.size 14
                        , E.spacing 8
                        , E.alignTop
                        , E.paddingXY 15 15
                        , E.scrollbarY
                        ]
                        (viewPublicDocuments model)
                    ]
        )



-- TOP
--


searchStatus model =
    let
        n =
            List.length model.foundIds

        i =
            if model.foundIdIndex == 0 then
                n

            else
                model.foundIdIndex

        msg =
            if n > 0 then
                String.fromInt i ++ "/" ++ String.fromInt n

            else
                ""
    in
    E.el [ Background.color (E.rgb 0.4 0.4 0.4), Font.color (E.rgb 1 1 1), Font.size 14, E.width (E.px 80), E.height (E.px 33) ]
        (E.el [ E.centerX, E.centerY ] (E.text msg))


viewDocumentsInIndex : DocPermissions -> Maybe Document -> List Document -> List (Element FrontendMsg)
viewDocumentsInIndex docPermissions currentDocument docs =
    List.map (Button.setDocumentInPhoneAsCurrent docPermissions currentDocument) docs


currentAuthor : Maybe Document -> String
currentAuthor maybeDoc =
    case maybeDoc of
        Nothing ->
            ""

        Just doc ->
            doc.author |> Maybe.withDefault ""


viewRendered : Model -> Int -> Element FrontendMsg
viewRendered model width_ =
    case model.currentDocument of
        Nothing ->
            E.none

        Just doc ->
            E.column
                [ E.paddingEach { left = 0, right = 0, top = 0, bottom = 0 }
                , View.Style.bgGray 1.0
                , E.width (E.px width_)
                , E.height (E.px (appHeight_ model))
                , Font.size 14
                , E.alignTop
                , E.scrollbarY
                , E.clipX
                , View.Utility.elementAttribute "id" "__RENDERED_TEXT__"
                ]
                [ View.Utility.katexCSS
                , E.column [ E.spacing 18, E.width (E.px width_), E.paddingXY 16 32 ]
                    ((Render.TOC.view model.counter (renderSettings model.windowWidth) model.ast |> E.map Render)
                        :: (Render.L0.renderFromAST model.counter (renderSettings (round <| 2.5 * toFloat model.windowWidth)) model.ast |> List.map (E.map Render))
                    )
                ]


viewPublicDocuments : Model -> List (Element FrontendMsg)
viewPublicDocuments model =
    viewDocumentsInIndex ReadOnly model.currentDocument model.publicDocuments


header model width_ =
    E.row [ E.spacing 12, E.width E.fill ]
        [ View.Input.searchDocsInput model
        , E.el [ Font.size 14, Font.color (E.rgb 0.9 0.9 0.9) ] (E.text (currentAuthor model.currentDocument))

        -- , E.el [ E.alignRight ] (title Config.appName)
        ]


setSelectedId : String -> Render.Settings.Settings -> Render.Settings.Settings
setSelectedId id settings =
    { settings | selectedId = id }


renderSettings : Int -> Render.Settings.Settings
renderSettings w =
    Render.Settings.makeSettings 0.38 w


viewPublicDocument : DocumentLink -> Element FrontendMsg
viewPublicDocument docLink =
    E.newTabLink [] { url = docLink.url, label = E.el [] (E.text (softTruncate softTruncateLimit docLink.label)) }


softTruncateLimit =
    50


softTruncate : Int -> String -> String
softTruncate k str =
    case String.Extra.softBreak 40 str of
        [] ->
            ""

        str2 :: [] ->
            str2

        str2 :: rest ->
            str2 ++ " ..."



--compile : Language -> Int -> Settings -> List String -> List (Element msg)
--compile language generation settings lines


renderArgs model =
    { width = panelWidth_ model - 140
    , selectedId = "foobar"
    , generation = 0
    }



-- DIMENSIONS


innerGutter =
    12


outerGutter =
    12


panelWidth_ ww =
    (appWidth ww - indexWidth ww) // 2 - innerGutter - outerGutter



-- BOTTOM


smallPanelWidth ww =
    smallAppWidth ww - innerGutter


indexWidth ww =
    ramp 150 300 ww


appWidth ww =
    ramp 700 1400 ww


smallAppWidth ww =
    -- ramp 700 1000 ww
    ww


ramp a b x =
    if x < a then
        a

    else if x > b then
        b

    else
        x


appHeight_ model =
    model.windowHeight


mainColumnStyle model =
    [ E.paddingEach { top = 0, bottom = 0, left = 0, right = 0 }
    , E.width (E.px model.windowWidth)
    , E.height (E.px model.windowHeight)
    ]


title : String -> Element msg
title str =
    E.row [ E.centerX, View.Style.fgGray 0.9 ] [ E.text str ]
