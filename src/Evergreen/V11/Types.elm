module Evergreen.V11.Types exposing (..)

import Browser
import Browser.Dom
import Browser.Navigation
import Debounce
import Dict
import Evergreen.V11.Abstract
import Evergreen.V11.Authentication
import Evergreen.V11.Document
import Evergreen.V11.Render.Msg
import Evergreen.V11.User
import File
import Http
import L0
import Random
import Time
import Url


type AppMode
    = UserMode
    | AdminMode


type PopupWindow
    = AdminPopup


type PopupStatus
    = PopupOpen PopupWindow
    | PopupClosed


type DocPermissions
    = ReadOnly
    | CanEdit


type PrintingState
    = PrintWaiting
    | PrintProcessing
    | PrintReady


type DocumentDeleteState
    = WaitingForDeleteAction
    | CanDelete


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , url : Url.Url
    , message : String
    , statusReport : List String
    , inputSpecial : String
    , currentUser : Maybe Evergreen.V11.User.User
    , inputUsername : String
    , inputPassword : String
    , appMode : AppMode
    , windowWidth : Int
    , windowHeight : Int
    , popupStatus : PopupStatus
    , showEditor : Bool
    , authorId : String
    , foundIds : List String
    , foundIdIndex : Int
    , selectedId : String
    , syncRequestIndex : Int
    , sourceText : String
    , ast : L0.AST
    , searchCount : Int
    , searchSourceText : String
    , lineNumber : Int
    , permissions : DocPermissions
    , debounce : Debounce.Debounce String
    , currentDocument : Maybe Evergreen.V11.Document.Document
    , documents : List Evergreen.V11.Document.Document
    , inputSearchKey : String
    , printingState : PrintingState
    , documentDeleteState : DocumentDeleteState
    , counter : Int
    , publicDocuments : List Evergreen.V11.Document.Document
    , deleteDocumentState : DocumentDeleteState
    }


type alias DocumentDict =
    Dict.Dict String Evergreen.V11.Document.Document


type alias AuthorDict =
    Dict.Dict String String


type alias PublicIdDict =
    Dict.Dict String String


type alias AbstractDict =
    Dict.Dict String Evergreen.V11.Abstract.Abstract


type alias UserId =
    String


type alias DocId =
    String


type alias UsersDocumentsDict =
    Dict.Dict UserId (List DocId)


type alias BackendModel =
    { message : String
    , currentTime : Time.Posix
    , randomSeed : Random.Seed
    , uuidCount : Int
    , randomAtmosphericInt : Maybe Int
    , authenticationDict : Evergreen.V11.Authentication.AuthenticationDict
    , documentDict : DocumentDict
    , authorIdDict : AuthorDict
    , publicIdDict : PublicIdDict
    , abstractDict : AbstractDict
    , usersDocumentsDict : UsersDocumentsDict
    , publicDocuments : List Evergreen.V11.Document.Document
    , documents : List Evergreen.V11.Document.Document
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | SetAppMode AppMode
    | GotNewWindowDimensions Int Int
    | GotViewport Browser.Dom.Viewport
    | SetViewPortForElement (Result Browser.Dom.Error ( Browser.Dom.Element, Browser.Dom.Viewport ))
    | ChangePopupStatus PopupStatus
    | CloseEditor
    | OpenEditor
    | InputSpecial String
    | RunSpecial
    | ExportJson
    | JsonRequested
    | JsonSelected File.File
    | JsonLoaded String
    | SignIn
    | SignOut
    | InputUsername String
    | InputPassword String
    | SyncLR
    | SendSyncLR
    | GetSelection String
    | InputSearchSource String
    | InputText String
    | DebounceMsg Debounce.Msg
    | Saved String
    | InputSearchKey String
    | Search
    | InputAuthorId String
    | NewDocument
    | SetDocumentAsCurrent DocPermissions Evergreen.V11.Document.Document
    | SetPublic Evergreen.V11.Document.Document Bool
    | AskFoDocumentById String
    | AskForDocumentByAuthorId
    | DeleteDocument
    | SetDeleteDocumentState DocumentDeleteState
    | Render Evergreen.V11.Render.Msg.MarkupMsg
    | ExportToMarkdown
    | ExportToLaTeX
    | Export
    | PrintToPDF
    | GotPdfLink (Result Http.Error String)
    | ChangePrintingState PrintingState
    | FinallyDoCleanPrintArtefacts String
    | Help String


type alias BackupOLD =
    { message : String
    , currentTime : Time.Posix
    , randomSeed : Random.Seed
    , uuidCount : Int
    , randomAtmosphericInt : Maybe Int
    , authenticationDict : Evergreen.V11.Authentication.AuthenticationDict
    , documentDict : DocumentDict
    , authorIdDict : AuthorDict
    , publicIdDict : PublicIdDict
    , abstractDict : AbstractDict
    , usersDocumentsDict : UsersDocumentsDict
    , publicDocuments : List Evergreen.V11.Document.Document
    , documents : List Evergreen.V11.Document.Document
    }


type ToBackend
    = NoOpToBackend
    | GetBackupData
    | RunTask
    | GetStatus
    | RestoreBackup BackupOLD
    | SignInOrSignUp String String
    | FetchDocumentById String
    | GetPublicDocuments
    | SaveDocument (Maybe Evergreen.V11.User.User) Evergreen.V11.Document.Document
    | GetDocumentByAuthorId String
    | GetDocumentByPublicId String
    | CreateDocument (Maybe Evergreen.V11.User.User) Evergreen.V11.Document.Document
    | StealDocument Evergreen.V11.User.User String
    | SearchForDocuments (Maybe String) String
    | DeleteDocumentBE Evergreen.V11.Document.Document


type BackendMsg
    = NoOpBackendMsg
    | GotAtomsphericRandomNumber (Result Http.Error String)
    | Tick Time.Posix


type ToFrontend
    = NoOpToFrontend
    | SendBackupData String
    | SendUser Evergreen.V11.User.User
    | SendDocument DocPermissions Evergreen.V11.Document.Document
    | SendDocuments (List Evergreen.V11.Document.Document)
    | SendMessage String
    | StatusReport (List String)
    | SetShowEditor Bool
    | GotPublicDocuments (List Evergreen.V11.Document.Document)
