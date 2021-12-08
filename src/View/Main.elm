module View.Main exposing (view)

import Config
import Document exposing (Access(..), Document)
import Element as E exposing (Element)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as HtmlAttr exposing (attribute)
import Html.Events
import Json.Decode
import Markup.API
import Render.Msg
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
        (mainColumn model)


mainColumn : Model -> Element FrontendMsg
mainColumn model =
    case model.appMode of
        AdminMode ->
            viewAdmin model

        UserMode ->
            if model.showEditor then
                viewEditorAndRenderedText model

            else if model.statusReport == [] then
                viewRenderedTextOnly model

            else
                viewStatusReport model



-- TOP


viewAdmin : Model -> Element FrontendMsg
viewAdmin model =
    E.column (mainColumnStyle model)
        [ E.column [ E.spacing 12, E.centerX, E.width (E.px <| appWidth model.windowWidth), E.height (E.px (appHeight_ model)) ]
            [ header model (E.px <| appWidth model.windowWidth)
            , E.row [ E.spacing 12 ]
                [ View.Utility.showIf (isAdmin model) (View.Input.specialInput model)
                , Button.runSpecial
                , Button.toggleAppMode model
                , Button.exportJson
                , View.Utility.showIf (isAdmin model) Button.importJson
                ]
            , footer model (appWidth model.windowWidth)
            ]
        ]


viewEditorAndRenderedText : Model -> Element FrontendMsg
viewEditorAndRenderedText model =
    E.column (mainColumnStyle model)
        [ E.column [ E.spacing 12, E.centerX, E.width (E.px <| appWidth model.windowWidth), E.height (E.px (appHeight_ model)) ]
            [ header model (E.px <| appWidth model.windowWidth)
            , E.row [ E.spacing 12 ]
                [ -- viewEditor model (panelWidth_ model.windowWidth)
                  aceEditor model
                , viewRendered model (panelWidth_ model.windowWidth)
                , viewMydocs model 110
                ]
            , footer model (appWidth model.windowWidth)
            ]
        ]



--


aceEditor : Model -> Element FrontendMsg
aceEditor model =
    E.column [ E.moveUp 4 ]
        [ E.row [ E.width E.fill ]
            [ View.Input.searchSourceText model
            , Button.syncLR
            , searchStatus model
            ]
        , aceEditor_ model
        ]


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



--


aceEditor_ : Model -> Element FrontendMsg
aceEditor_ model =
    let
        onChange : Html.Attribute FrontendMsg
        onChange =
            Json.Decode.string
                |> Json.Decode.at [ "target", "editorText" ]
                |> Json.Decode.map InputText
                |> Html.Events.on "change"

        onSelect : Html.Attribute FrontendMsg
        onSelect =
            Json.Decode.string
                |> Json.Decode.at [ "target", "editorText" ]
                |> Json.Decode.map GetSelection
                |> Html.Events.on "selectedtext"
    in
    E.el [ E.htmlAttribute onChange, E.htmlAttribute onSelect ] <|
        E.html <|
            Html.node "ace-editor"
                [ --HtmlAttr.attribute "theme" "one_dark"
                  HtmlAttr.attribute "wrapmode" "true"
                , HtmlAttr.attribute "tabsize" "2"
                , HtmlAttr.attribute "linenumber" (String.fromInt (model.lineNumber + 1))
                , HtmlAttr.attribute "softtabs" "true"
                , HtmlAttr.attribute "navigateWithinSoftTabs" "true"
                , HtmlAttr.attribute "fontsize" "12"
                , HtmlAttr.style "height" (String.fromInt (panelHeight_ model - 40) ++ "px")
                , HtmlAttr.style "width" (String.fromInt (panelWidth_ model.windowWidth) ++ "px")
                , HtmlAttr.attribute "text" (Maybe.map .content model.currentDocument |> Maybe.withDefault "")
                , HtmlAttr.attribute "searchkey" model.searchSourceText
                , HtmlAttr.attribute "searchcount" (String.fromInt model.searchCount)

                --, HtmlAttr.attribute "sendsync" (String.fromInt model.syncRequestIndex)
                ]
                []



-- MIDDLE


viewRenderedTextOnly : Model -> Element FrontendMsg
viewRenderedTextOnly model =
    let
        deltaH =
            case model.currentUser of
                Nothing ->
                    110

                Just _ ->
                    (appHeight_ model - 100) // 2 + 110
    in
    E.column (mainColumnStyle model)
        [ E.column [ E.centerX, E.spacing 12, E.width (E.px <| smallAppWidth model.windowWidth), E.height (E.px (appHeight_ model)) ]
            [ header model (E.px <| smallHeaderWidth model.windowWidth)
            , E.row [ E.spacing 12 ]
                [ viewRenderedContainer model
                , E.column [ E.spacing 8 ]
                    [ hideIf (model.currentUser == Nothing) (viewMydocs model deltaH)
                    , viewZipdocs model deltaH
                    ]
                ]
            , footer model (smallHeaderWidth model.windowWidth)
            ]
        ]


viewRenderedContainer model =
    E.column [ E.spacing 18 ]
        [ viewRendered model (smallPanelWidth model.windowWidth)
        ]


viewMydocs : Model -> Int -> Element FrontendMsg
viewMydocs model deltaH =
    let
        docs =
            List.sortBy (\doc -> softTruncate softTruncateLimit doc.title) model.documents
    in
    E.column
        [ E.width (E.px <| indexWidth model.windowWidth)
        , E.height (E.px (appHeight_ model - deltaH))
        , Font.size 14
        , E.scrollbarY
        , Background.color (E.rgb 0.95 0.95 1.0)
        , E.paddingXY 12 18
        , Font.color (E.rgb 0.1 0.1 1.0)
        , E.spacing 8
        ]
        (E.el [ Font.size 16, Font.color (E.rgb 0.1 0.1 0.1) ] (E.text <| "My Docs (" ++ String.fromInt (List.length docs) ++ ")")
            :: viewDocumentsInIndex CanEdit
                model.currentDocument
                docs
        )


viewDocumentsInIndex : DocPermissions -> Maybe Document -> List Document -> List (Element FrontendMsg)
viewDocumentsInIndex docPermissions currentDocument docs =
    List.map (Button.setDocumentAsCurrent docPermissions currentDocument) docs


viewZipdocs model deltaH =
    E.column
        [ E.width (E.px <| indexWidth model.windowWidth)
        , E.height (E.px (appHeight_ model - deltaH))
        , Font.size 14
        , E.scrollbarY
        , Background.color (E.rgb 0.95 0.95 1.0)
        , E.paddingXY 12 18
        , Font.color (E.rgb 0.1 0.1 1.0)
        , E.spacing 8
        ]
        (E.el [ Font.size 16, Font.color (E.rgb 0.1 0.1 0.1) ] (E.text <| "Published Zipdocs (" ++ String.fromInt (List.length model.publicDocuments) ++ ")") :: viewPublicDocuments model)


footer model width_ =
    E.row
        [ E.spacing 12
        , E.paddingXY 0 8
        , E.height (E.px 25)
        , E.width E.fill -- (E.px width_)
        , Font.size 14
        ]
        [ Button.exportToLaTeX
        , Button.printToPDF model

        -- , View.Utility.showIf (isAdmin model) Button.runSpecial
        , View.Utility.showIf (isAdmin model) (Button.toggleAppMode model)

        -- , View.Utility.showIf (isAdmin model) Button.exportJson
        --, View.Utility.showIf (isAdmin model) Button.importJson
        -- , View.Utility.showIf (isAdmin model) (View.Input.specialInput model)
        , messageRow model
        ]


messageRowInset model =
    case model.currentUser of
        Nothing ->
            10

        Just user ->
            if user.username == "jxxcarlson" then
                260

            else
                10


isAdmin : Model -> Bool
isAdmin model =
    Maybe.map .username model.currentUser == Just "jxxcarlson"


messageRow model =
    E.row
        [ E.width E.fill
        , E.height (E.px 30)
        , E.paddingXY 8 4
        , View.Style.bgGray 0.1
        , View.Style.fgGray 1.0
        ]
        [ E.text model.message ]


header model width_ =
    E.row [ E.spacing 12, E.width E.fill ]
        [ Button.newDocument
        , Button.deleteDocument model
        , Button.cancelDeleteDocument model
        , View.Utility.showIf model.showEditor Button.closeEditor
        , View.Utility.hideIf (model.currentUser == Nothing || model.permissions == ReadOnly || model.showEditor) Button.openEditor
        , Button.miniLaTeXLanguageButton model
        , Button.markupLanguageButton model
        , View.Utility.showIf model.showEditor (Button.togglePublic model.currentDocument)

        -- , Button.l1LanguageButton model
        , View.Utility.showIf model.showEditor (wordCount model)
        , E.el [ Font.size 14, Font.color (E.rgb 0.9 0.9 0.9) ] (E.text (currentAuthor model.currentDocument))
        , View.Input.searchDocsInput model
        , View.Utility.showIf (model.currentUser == Nothing) Button.signIn
        , View.Utility.showIf (model.currentUser == Nothing) (View.Input.usernameInput model)
        , View.Utility.showIf (model.currentUser == Nothing) (View.Input.passwordInput model)
        , Button.signOut model

        -- , Button.help
        , E.el [ E.alignRight ] (title Config.appName)
        ]


currentAuthor : Maybe Document -> String
currentAuthor maybeDoc =
    case maybeDoc of
        Nothing ->
            ""

        Just doc ->
            doc.author |> Maybe.withDefault ""


wordCount : Model -> Element FrontendMsg
wordCount model =
    case model.currentDocument of
        Nothing ->
            E.none

        Just doc ->
            E.el [ Font.size 14, Font.color Color.lightGray ] (E.text <| "words: " ++ (String.fromInt <| Document.wordCount doc))


viewEditor : Model -> Int -> Element FrontendMsg
viewEditor model width_ =
    E.column
        [ E.alignTop
        , E.spacing 8
        ]
        [ viewEditor_ model width_
        ]


viewEditor_ : Model -> Int -> Element FrontendMsg
viewEditor_ model width_ =
    case model.currentDocument of
        Nothing ->
            E.none

        Just doc ->
            Input.multiline
                [ E.height (E.px (panelHeight_ model))
                , E.width (E.px width_)
                , E.width (E.px width_)
                , Font.size 14
                , Background.color (E.rgb255 240 241 255)
                ]
                { onChange = InputText
                , text = model.sourceText
                , placeholder = Nothing
                , label = Input.labelHidden "Enter source text here"
                , spellcheck = False
                }


viewRendered : Model -> Int -> Element FrontendMsg
viewRendered model width_ =
    case model.currentDocument of
        Nothing ->
            E.none

        Just doc ->
            E.column
                [ E.paddingEach { left = 24, right = 24, top = 32, bottom = 96 }
                , View.Style.bgGray 1.0
                , E.width (E.px width_)
                , E.height (E.px (panelHeight_ model))
                , Font.size 14
                , E.alignTop
                , E.scrollbarY
                , View.Utility.elementAttribute "id" "__RENDERED_TEXT__"
                ]
                [ View.Utility.katexCSS
                , E.column [ E.spacing 18, E.width (E.px (width_ - 60)) ]
                    (Markup.API.renderFancy (settings model.selectedId) doc.language model.counter (String.lines doc.content) |> List.map (E.map Render))
                ]


viewPublicDocuments : Model -> List (Element FrontendMsg)
viewPublicDocuments model =
    viewDocumentsInIndex ReadOnly model.currentDocument model.publicDocuments


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


settings : String -> Markup.API.Settings
settings selectedId =
    { width = 500
    , titleSize = 30
    , showTOC = True
    , showErrorMessages = True
    , paragraphSpacing = 14
    , selectedId = selectedId
    }


viewStatusReport model =
    E.column (mainColumnStyle model)
        [ E.column [ E.centerX, E.spacing 12, E.width (E.px <| smallAppWidth model.windowWidth), E.height (E.px (appHeight_ model)) ]
            [ header model (E.px <| smallAppWidth model.windowWidth)
            , E.column [ E.spacing 8, E.paddingXY 12 12, Font.size 14, Background.color (E.rgb 1 1 1), E.width (E.px (smallAppWidth model.windowWidth)) ]
                (List.map (\item -> E.el [] (E.text item)) model.statusReport)
            , footer model (smallAppWidth model.windowWidth)

            --, footer model 400
            ]
        ]



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
    smallAppWidth ww - indexWidth ww - innerGutter


smallHeaderWidth ww =
    smallAppWidth ww


headerWidth ww =
    appWidth ww - 2 * innerGutter


indexWidth ww =
    ramp 150 300 ww


appWidth ww =
    ramp 700 1400 ww


smallAppWidth ww =
    ramp 700 1000 ww


docListWidth =
    220


ramp a b x =
    if x < a then
        a

    else if x > b then
        b

    else
        x


appHeight_ model =
    model.windowHeight - 50


panelHeight_ model =
    appHeight_ model - 110


appWidth_ model =
    model.windowWidth


mainColumnStyle model =
    [ View.Style.bgGray 0.5
    , E.paddingEach { top = 40, bottom = 20, left = 0, right = 0 }
    , E.width (E.px model.windowWidth)
    , E.height (E.px model.windowHeight)
    ]


title : String -> Element msg
title str =
    E.row [ E.centerX, View.Style.fgGray 0.9 ] [ E.text str ]
