module Frontend exposing (..)

import Authentication
import Backend.Backup
import Block.Accumulator
import Browser exposing (UrlRequest(..))
import Browser.Events
import Browser.Navigation as Nav
import Cmd.Extra exposing (withCmd, withCmds, withNoCmd)
import Config
import Data
import Debounce exposing (Debounce)
import Docs
import Document exposing (Access(..))
import Expression.ASTTools
import File
import File.Download as Download
import File.Select as Select
import Frontend.Cmd
import Frontend.PDF as PDF
import Frontend.Update
import Html exposing (Html)
import LaTeX.Export.API
import Lamdera exposing (sendToBackend)
import Lang.Lang as Lang exposing (Lang(..))
import List.Extra
import Markup.API
import Process
import Render.Msg exposing (MarkupMsg(..))
import Task
import Types exposing (..)
import Url exposing (Url)
import UrlManager
import User
import Util
import View.Main
import View.Utility


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \m -> Sub.none
        , view = view
        }


subscriptions model =
    Sub.batch
        [ Browser.Events.onResize (\w h -> GotNewWindowDimensions w h)
        ]


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( { key = key
      , url = url
      , message = "Welcome!"

      -- ADMIN
      , statusReport = []
      , inputSpecial = ""

      -- USER
      , currentUser = Nothing
      , inputUsername = ""
      , inputPassword = ""

      -- UI
      , appMode = UserMode
      , windowWidth = 600
      , windowHeight = 900
      , popupStatus = PopupClosed
      , showEditor = False

      -- SYNC
      , foundIds = []
      , foundIdIndex = 0
      , selectedId = ""
      , searchCount = 0
      , searchSourceText = ""
      , syncRequestIndex = 0

      -- DOCUMENT
      , parseData = { ast = [], accumulator = Block.Accumulator.init 4 }
      , lineNumber = 0
      , permissions = ReadOnly
      , sourceText = ""
      , debounce = Debounce.init
      , counter = 0
      , inputSearchKey = ""
      , authorId = ""
      , documents = []
      , currentDocument = Just Docs.notSignedIn
      , printingState = PrintWaiting
      , documentDeleteState = WaitingForDeleteAction
      , language = Lang.MiniLaTeX
      , publicDocuments = []
      , deleteDocumentState = WaitingForDeleteAction
      }
    , Cmd.batch [ Frontend.Cmd.setupWindow, urlAction url.path, sendToBackend GetPublicDocuments ]
    )


debounceConfig : Debounce.Config FrontendMsg
debounceConfig =
    { strategy = Debounce.soon 300
    , transform = DebounceMsg
    }


urlAction path =
    let
        prefix =
            String.left 3 path

        id =
            String.dropLeft 3 path
    in
    if path == "/status/69a1c3be-4971-4673-9e0f-95456fd709a6" then
        sendToBackend GetStatus

    else
        case prefix of
            "/p/" ->
                sendToBackend (GetDocumentByPublicId id)

            "/a/" ->
                sendToBackend (GetDocumentByAuthorId id)

            "/status/69a1c3be-4971-4673-9e0f-95456fd709a6" ->
                sendToBackend GetStatus

            _ ->
                Cmd.none


urlIsForGuest : Url -> Bool
urlIsForGuest url =
    String.left 2 url.path == "/g"


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    let
                        cmd =
                            case .fragment url of
                                Just internalId ->
                                    -- internalId is the part after '#', if present
                                    View.Utility.setViewportForElement internalId

                                Nothing ->
                                    --if String.left 3 url.path == "/a/" then
                                    sendToBackend (GetDocumentByAuthorId (String.dropLeft 3 url.path))

                        --
                        --else if String.left 3 url.path == "/p/" then
                        --    sendToBackend (GetDocumentByPublicId (String.dropLeft 3 url.path))
                        --
                        --else
                        --    Nav.pushUrl model.key (Url.toString url)
                    in
                    ( model, cmd )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            -- ( model, Cmd.none )
            ( { model | url = url }
            , Cmd.batch
                [ UrlManager.handleDocId url
                ]
            )

        -- USER
        SignIn ->
            if String.length model.inputPassword >= 8 then
                ( model
                , sendToBackend (SignInOrSignUp model.inputUsername (Authentication.encryptForTransit model.inputPassword))
                )

            else
                ( { model | message = "Password must be at least 8 letters long." }, Cmd.none )

        SignOut ->
            ( { model
                | currentUser = Nothing
                , currentDocument = Just Docs.notSignedIn
                , documents = []
                , message = "Signed out"
                , inputSearchKey = ""
                , inputUsername = ""
                , inputPassword = ""
                , showEditor = False
              }
            , -- Cmd.none
              Nav.pushUrl model.key "/"
            )

        -- ADMIN
        ExportJson ->
            ( model, sendToBackend GetBackupData )

        JsonRequested ->
            ( model, Select.file [ "text/json" ] JsonSelected )

        JsonSelected file ->
            ( model, Task.perform JsonLoaded (File.toString file) )

        JsonLoaded jsonImport ->
            case Backend.Backup.decodeBackup jsonImport of
                Err _ ->
                    ( { model | message = "Error decoding backup" }, Cmd.none )

                Ok backendModel ->
                    ( { model | message = "restoring backup ..." }, sendToBackend (RestoreBackup backendModel) )

        InputSpecial str ->
            { model | inputSpecial = str } |> withNoCmd

        RunSpecial ->
            case model.currentUser of
                Nothing ->
                    model |> withNoCmd

                Just user ->
                    if user.username == "jxxcarlson" then
                        model |> withCmd (sendToBackend (StealDocument user model.inputSpecial))

                    else
                        model |> withNoCmd

        InputUsername str ->
            ( { model | inputUsername = str }, Cmd.none )

        InputPassword str ->
            ( { model | inputPassword = str }, Cmd.none )

        -- UI
        SetAppMode appMode ->
            ( { model | appMode = appMode }, Cmd.none )

        GotNewWindowDimensions w h ->
            ( { model | windowWidth = w, windowHeight = h }, Cmd.none )

        GotViewport vp ->
            Frontend.Update.updateWithViewport vp model

        SetViewPortForElement result ->
            case result of
                Ok ( element, viewport ) ->
                    ( { model | message = model.message ++ ", setting viewport" }, View.Utility.setViewPortForSelectedLine element viewport )

                Err _ ->
                    ( { model | message = model.message ++ ", could not set viewport" }, Cmd.none )

        InputSearchSource str ->
            ( { model | searchSourceText = str, foundIdIndex = 0 }, Cmd.none )

        GetSelection str ->
            ( { model | message = "Selection: " ++ str }, Cmd.none )

        SendSyncLR ->
            ( { model | syncRequestIndex = model.syncRequestIndex + 1 }, Cmd.none )

        SyncLR ->
            let
                data =
                    if model.foundIdIndex == 0 then
                        let
                            foundIds_ =
                                Expression.ASTTools.findIdsMatchingText model.searchSourceText model.parseData.ast |> List.map fixId_

                            id_ =
                                List.head foundIds_ |> Maybe.withDefault "(nothing)"
                        in
                        { foundIds = foundIds_
                        , foundIdIndex = 1
                        , cmd = View.Utility.setViewportForElement (id_ ++ ".0")
                        , selectedId = id_
                        , searchCount = 0
                        }

                    else
                        let
                            id_ =
                                List.Extra.getAt model.foundIdIndex model.foundIds |> Maybe.withDefault "(nothing)"
                        in
                        { foundIds = model.foundIds
                        , foundIdIndex = modBy (List.length model.foundIds) (model.foundIdIndex + 1)
                        , cmd = View.Utility.setViewportForElement (id_ ++ ".0")
                        , selectedId = id_
                        , searchCount = model.searchCount + 1
                        }
            in
            ( { model
                | selectedId = data.selectedId
                , foundIds = data.foundIds
                , foundIdIndex = data.foundIdIndex
                , searchCount = data.searchCount
                , message = ("[" ++ data.selectedId ++ "]") :: data.foundIds |> String.join ", "
              }
            , data.cmd
            )

        ChangePopupStatus status ->
            ( { model | popupStatus = status }, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        CloseEditor ->
            ( { model | showEditor = False }, sendToBackend GetPublicDocuments )

        OpenEditor ->
            ( { model | showEditor = True }, Cmd.none )

        Help docId ->
            ( model, sendToBackend (GetDocumentByAuthorId docId) )

        -- DOCUMENT
        Render msg_ ->
            case msg_ of
                Render.Msg.SendMeta m ->
                    ( { model | lineNumber = m.loc.begin.row, message = "line " ++ String.fromInt (m.loc.begin.row + 1) }, Cmd.none )

                GetPublicDocument id ->
                    ( model, sendToBackend (FetchDocumentById id) )

        DebounceMsg msg_ ->
            let
                ( debounce, cmd ) =
                    Debounce.update
                        debounceConfig
                        (Debounce.takeLast save)
                        msg_
                        model.debounce
            in
            ( { model | debounce = debounce }
            , cmd
            )

        Saved str ->
            updateDoc model str

        Search ->
            ( model, sendToBackend (SearchForDocuments (model.currentUser |> Maybe.map .username) model.inputSearchKey) )

        InputText str ->
            -- updateDoc model str
            let
                -- Push your values here.
                ( debounce, cmd ) =
                    Debounce.push debounceConfig str model.debounce
            in
            ( { model
                | sourceText = str
                , debounce = debounce
              }
            , cmd
            )

        InputAuthorId str ->
            ( { model | authorId = str }, Cmd.none )

        AskFoDocumentById id ->
            ( model, sendToBackend (GetDocumentByAuthorId id) )

        AskForDocumentByAuthorId ->
            ( model, sendToBackend (GetDocumentByAuthorId model.authorId) )

        InputSearchKey str ->
            ( { model | inputSearchKey = str }, Cmd.none )

        NewDocument ->
            Frontend.Update.newDocument model

        SetDeleteDocumentState s ->
            ( { model | deleteDocumentState = s }, Cmd.none )

        DeleteDocument ->
            case model.currentDocument of
                Nothing ->
                    ( model, Cmd.none )

                Just doc ->
                    ( { model
                        | currentDocument = Just Docs.deleted
                        , language = Markdown
                        , documents = List.filter (\d -> d.id /= doc.id) model.documents
                        , deleteDocumentState = WaitingForDeleteAction
                      }
                    , sendToBackend (DeleteDocumentBE doc)
                    )

        SetDocumentAsCurrent permissions doc ->
            ( { model
                | currentDocument = Just doc
                , sourceText = doc.content
                , parseData = Markup.API.parse model.language model.counter (String.lines doc.content)
                , message = Config.appUrl ++ "/p/" ++ doc.publicId ++ ", id = " ++ doc.id
                , permissions = setPermissions model.currentUser permissions doc
                , counter = model.counter + 1
              }
            , View.Utility.setViewPortToTop
            )

        SetLanguage lang ->
            ( { model | language = lang }, Cmd.none )

        SetPublic doc public ->
            let
                newDocument =
                    { doc | public = public }

                documents =
                    List.Extra.setIf (\d -> d.id == newDocument.id) newDocument model.documents
            in
            ( { model | documents = documents, currentDocument = Just newDocument }, sendToBackend (SaveDocument model.currentUser newDocument) )

        ExportToMarkdown ->
            let
                markdownText =
                    -- TODO:implement this
                    -- L1.Render.Markdown.transformDocument model.currentDocument.content
                    "Not implemented"

                fileName_ =
                    "foo" |> String.replace " " "-" |> String.toLower |> (\name -> name ++ ".md")
            in
            ( model, Download.string fileName_ "text/markdown" markdownText )

        ExportToLaTeX ->
            issueCommandIfDefined model.currentDocument model (exportToLaTeX model.language)

        Export ->
            issueCommandIfDefined model.currentDocument model exportDoc

        --let
        --    fileName =
        --        "doc" |> String.replace " " "-" |> String.toLower |> (\name -> name ++ ".l1")
        --in
        --( model, Download.string fileName "text/plain" model.currentDocument.content )
        PrintToPDF ->
            PDF.print model

        GotPdfLink result ->
            PDF.gotLink model result

        ChangePrintingState printingState ->
            -- TODO: review this
            issueCommandIfDefined model.currentDocument { model | printingState = printingState } (changePrintingState printingState)

        FinallyDoCleanPrintArtefacts privateId ->
            ( model, Cmd.none )


fixId_ : String -> String
fixId_ str =
    -- TODO: Review this. We should not have to do this
    let
        parts =
            String.split "." str
    in
    case
        List.head parts
    of
        Nothing ->
            str

        Just prefix ->
            let
                p =
                    String.toInt prefix |> Maybe.withDefault 0 |> (\x -> x + 1) |> String.fromInt
            in
            (p :: List.drop 1 parts) |> String.join "."


setPermissions currentUser permissions document =
    case document.author of
        Nothing ->
            permissions

        Just author ->
            if Just author == Maybe.map .username currentUser then
                CanEdit

            else
                permissions


save : String -> Cmd FrontendMsg
save s =
    Task.perform Saved (Task.succeed s)


updateDoc model str =
    case model.currentDocument of
        Nothing ->
            ( model, Cmd.none )

        Just doc ->
            let
                parseData =
                    Markup.API.parse doc.language model.counter (String.lines doc.content)

                newTitle =
                    Expression.ASTTools.getItem "title" parseData.ast |> Maybe.withDefault "Untitled"

                newDocument =
                    { doc | content = str, title = newTitle }

                documents =
                    List.Extra.setIf (\d -> d.id == newDocument.id) newDocument model.documents
            in
            ( { model
                | currentDocument = Just newDocument
                , parseData = parseData
                , counter = model.counter + 1
                , documents = documents
              }
            , sendToBackend (SaveDocument model.currentUser newDocument)
            )


changePrintingState printingState doc =
    if printingState == PrintWaiting then
        Process.sleep 1000 |> Task.perform (always (FinallyDoCleanPrintArtefacts doc.id))

    else
        Cmd.none


exportToLaTeX : Lang.Lang -> Document.Document -> Cmd msg
exportToLaTeX lang doc =
    let
        laTeXText =
            LaTeX.Export.API.export lang doc.content

        fileName =
            doc.id ++ fileExtension doc.language
    in
    Download.string fileName "application/x-latex" laTeXText


exportDoc : Document.Document -> Cmd msg
exportDoc doc =
    let
        fileName =
            doc.id ++ fileExtension doc.language
    in
    Download.string fileName "text/plain" doc.content


fileExtension : Lang.Lang -> String
fileExtension lang =
    case lang of
        L1 ->
            ".l1"

        MiniLaTeX ->
            ".tex"

        Markdown ->
            ".md"


issueCommandIfDefined : Maybe a -> Model -> (a -> Cmd msg) -> ( Model, Cmd msg )
issueCommandIfDefined maybeSomething model cmdMsg =
    case maybeSomething of
        Nothing ->
            ( model, Cmd.none )

        Just something ->
            ( model, cmdMsg something )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        -- DOCUMENT
        SendDocument access doc ->
            let
                documents =
                    Util.insertInList doc model.documents

                message =
                    "Documents: " ++ String.fromInt (List.length documents)

                showEditor =
                    case access of
                        ReadOnly ->
                            False

                        CanEdit ->
                            True
            in
            ( { model
                | sourceText = doc.content
                , showEditor = showEditor
                , currentDocument = Just doc
                , language = doc.language
                , documents = documents
              }
            , Cmd.none
            )

        GotPublicDocuments publicDocuments ->
            ( { model | publicDocuments = publicDocuments }, Cmd.none )

        SendMessage message ->
            ( { model | message = message }, Cmd.none )

        -- ADMIN
        SendBackupData data ->
            ( { model | message = "Backup data: " ++ String.fromInt (String.length data) ++ " chars" }, Download.string "zipdocs.json" "text/json" data )

        StatusReport items ->
            ( { model | statusReport = items }, Cmd.none )

        SetShowEditor flag ->
            ( { model | showEditor = flag }, Cmd.none )

        -- USER
        SendUser user ->
            ( { model | currentUser = Just user }, Cmd.none )

        SendDocuments documents ->
            ( { model | documents = documents }, Cmd.none )


view : Model -> { title : String, body : List (Html.Html FrontendMsg) }
view model =
    { title = Config.appName
    , body =
        [ View.Main.view model ]
    }
