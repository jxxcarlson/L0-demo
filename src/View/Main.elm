module View.Main exposing (view)

import Config
import Document exposing (Access(..), Document)
import Either exposing (Either(..))
import Element as E exposing (Element)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Element.Keyed
import Html exposing (Html)
import Html.Attributes as HtmlAttr exposing (attribute)
import Html.Events
import Json.Decode
import Render.L0
import Render.Settings
import Render.TOC
import String.Extra
import Time
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

            else
                viewRenderedTextOnly model



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
    let
        deltaH =
            (appHeight_ model - 100) // 2 + 130
    in
    E.column (mainColumnStyle model)
        [ E.column [ E.spacing 12, E.centerX, E.width (E.px <| appWidth model.windowWidth), E.height (E.px (appHeight_ model)) ]
            [ header model (E.px <| appWidth model.windowWidth)
            , E.row [ E.spacing 12 ]
                [ editor_ model
                , viewRenderedForEditor model (panelWidth_ model.windowWidth)
                , E.column [ E.spacing 8 ]
                    [ E.row [ E.spacing 12 ] [ Button.setSortModeMostRecent model.sortMode, Button.setSortModeAlpha model.sortMode ]
                    , viewMydocs model deltaH
                    , viewPublicDocs model deltaH
                    ]
                ]
            , footer model (appWidth model.windowWidth)
            ]
        ]


editor_ : Model -> Element FrontendMsg
editor_ model =
    Element.Keyed.el
        [ E.htmlAttribute onSelectionChange
        , E.htmlAttribute onTextChange
        , htmlId "editor-here"
        , E.width (E.px 550)
        , E.height (E.px (appHeight_ model - 110))
        , E.width (E.px (panelWidth_ model.windowWidth))
        , Background.color (E.rgb255 0 68 85)
        , Font.color (E.rgb 0.85 0.85 0.85)
        , Font.size 12
        ]
        ( stringOfBool model.showEditor
        , E.html
            (Html.node "codemirror-editor"
                [ HtmlAttr.attribute "text" model.initialText
                , HtmlAttr.attribute "linenumber" (String.fromInt model.linenumber)
                , HtmlAttr.attribute "selection" (stringOfBool model.doSync)
                ]
                []
            )
        )


stringOfBool bool =
    case bool of
        False ->
            "false"

        True ->
            "true"


htmlId str =
    E.htmlAttribute (HtmlAttr.id str)


onTextChange : Html.Attribute FrontendMsg
onTextChange =
    textDecoder
        |> Json.Decode.map InputText
        |> Html.Events.on "text-change"


onSelectionChange : Html.Attribute FrontendMsg
onSelectionChange =
    textDecoder
        |> Json.Decode.map SelectedText
        |> Html.Events.on "selected-text"


textDecoder : Json.Decode.Decoder String
textDecoder =
    Json.Decode.string
        |> Json.Decode.at [ "detail" ]



-- MIDDLE


viewRenderedTextOnly : Model -> Element FrontendMsg
viewRenderedTextOnly model =
    let
        deltaH =
            (appHeight_ model - 100) // 2 + 130
    in
    E.column (mainColumnStyle model)
        [ E.column [ E.centerX, E.spacing 12, E.width (E.px <| smallAppWidth model.windowWidth), E.height (E.px (appHeight_ model)) ]
            [ header model (E.px <| smallHeaderWidth model.windowWidth)
            , E.row [ E.spacing 12 ]
                [ viewRenderedContainer model
                , E.column [ E.spacing 8 ]
                    [ E.row [ E.spacing 12 ] [ Button.setSortModeMostRecent model.sortMode, Button.setSortModeAlpha model.sortMode ]
                    , viewMydocs model deltaH
                    , viewPublicDocs model deltaH
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
        sort =
            case model.sortMode of
                SortAlphabetically ->
                    List.sortBy (\doc -> softTruncate softTruncateLimit doc.title)

                SortByMostRecent ->
                    List.sortWith (\a b -> compare (Time.posixToMillis b.modified) (Time.posixToMillis a.modified))

        docs =
            sort model.documents
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


viewPublicDocs model deltaH =
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
        (E.el [ Font.size 16, Font.color (E.rgb 0.1 0.1 0.1) ] (E.text <| "Published docs (" ++ String.fromInt (List.length model.publicDocuments) ++ ")") :: viewPublicDocuments model)


footer model width_ =
    E.row
        [ E.spacing 12
        , E.paddingXY 0 8
        , E.height (E.px 25)
        , E.width E.fill -- (E.px width_)
        , Font.size 14
        ]
        [ Button.syncButton
        , Button.nextSyncButton model.foundIds
        , Button.exportToLaTeX
        , Button.printToPDF model

        -- , View.Utility.showIf (isAdmin model) Button.runSpecial
        , View.Utility.showIf (isAdmin model) (Button.toggleAppMode model)

        -- , View.Utility.showIf (isAdmin model) Button.exportJson
        --, View.Utility.showIf (isAdmin model) Button.importJson
        -- , View.Utility.showIf (isAdmin model) (View.Input.specialInput model)
        , E.el [ E.width E.fill, rightPaddingFooter model.showEditor ] (messageRow model)
        ]


rightPaddingFooter showEditor =
    case showEditor of
        True ->
            E.paddingEach { left = 0, right = 22, top = 0, bottom = 0 }

        False ->
            E.paddingEach { left = 0, right = 0, top = 0, bottom = 0 }


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
        [ View.Utility.hideIf (model.currentUser == Nothing) Button.newDocument
        , View.Utility.hideIf (model.currentUser == Nothing) (Button.deleteDocument model)
        , View.Utility.hideIf (model.currentUser == Nothing) (Button.cancelDeleteDocument model)
        , View.Utility.showIf model.showEditor Button.closeEditor
        , View.Utility.hideIf model.showEditor Button.openEditor
        , View.Utility.hideIf (model.currentUser == Nothing) (View.Utility.showIf model.showEditor (Button.togglePublic model.currentDocument))
        , View.Utility.showIf model.showEditor (wordCount model)
        , E.el [ Font.size 14, Font.color (E.rgb 0.9 0.9 0.9) ] (E.text (currentAuthor model.currentDocument))
        , View.Input.searchDocsInput model
        , Button.iLink Config.welcomeDocId "Home"
        , View.Utility.showIf (model.currentUser == Nothing) Button.signIn
        , View.Utility.showIf (model.currentUser == Nothing) (View.Input.usernameInput model)
        , View.Utility.showIf (model.currentUser == Nothing) (View.Input.passwordInput model)
        , Button.signOut model

        -- , Button.help
        , E.el [ E.alignRight, rightPaddingHeader model.showEditor ] (title Config.appName)
        ]


rightPaddingHeader showEditor =
    case showEditor of
        True ->
            E.paddingEach { left = 0, right = 30, top = 0, bottom = 0 }

        False ->
            E.paddingEach { left = 0, right = 0, top = 0, bottom = 0 }


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
                    ((Render.TOC.view model.counter (renderSettings model.windowWidth) model.ast |> E.map Render)
                        :: (Render.L0.renderFromAST model.counter (renderSettings model.windowWidth) model.ast |> List.map (E.map Render))
                    )
                ]


viewRenderedForEditor : Model -> Int -> Element FrontendMsg
viewRenderedForEditor model width_ =
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
                    ((Render.TOC.view model.counter (renderSettings model.windowWidth |> setSelectedId model.selectedId) model.ast |> E.map Render)
                        :: (Render.L0.renderFromAST model.counter (editorRenderSettings model.windowWidth |> setSelectedId model.selectedId) model.ast |> List.map (E.map Render))
                    )
                ]


setSelectedId : String -> Render.Settings.Settings -> Render.Settings.Settings
setSelectedId id settings =
    { settings | selectedId = id }


renderSettings : Int -> Render.Settings.Settings
renderSettings w =
    Render.Settings.makeSettings 0.38 w


editorRenderSettings : Int -> Render.Settings.Settings
editorRenderSettings w =
    Render.Settings.makeSettings 0.28 w


viewPublicDocuments : Model -> List (Element FrontendMsg)
viewPublicDocuments model =
    let
        sorter =
            case model.sortMode of
                SortAlphabetically ->
                    List.sortBy (\doc -> softTruncate softTruncateLimit doc.title)

                SortByMostRecent ->
                    List.sortWith (\a b -> compare (Time.posixToMillis b.modified) (Time.posixToMillis a.modified))
    in
    viewDocumentsInIndex ReadOnly model.currentDocument (sorter model.publicDocuments)


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


viewStatusReport model =
    E.column (mainColumnStyle model)
        [ E.column [ E.centerX, E.spacing 12, E.width (E.px <| smallAppWidth model.windowWidth), E.height (E.px (appHeight_ model)) ]
            [ header model (E.px <| smallAppWidth model.windowWidth)
            , E.column [ E.spacing 8, E.paddingXY 12 12, Font.size 14, Background.color (E.rgb 1 1 1), E.width (E.px (smallAppWidth model.windowWidth)) ]
                (List.map (\item -> E.el [] (E.text item)) model.statusReport)
            , footer model (smallAppWidth model.windowWidth)
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
