module Evergreen.V167.Types exposing (..)

import Browser
import Browser.Dom
import Browser.Navigation
import Debounce
import Dict
import Evergreen.V167.Abstract
import Evergreen.V167.Authentication
import Evergreen.V167.Compiler.DifferentialParser
import Evergreen.V167.Document
import Evergreen.V167.L0
import Evergreen.V167.Parser.Block
import Evergreen.V167.Render.Msg
import Evergreen.V167.User
import File
import Http
import Random
import Time
import Tree
import Url


type AppMode
    = UserMode
    | AdminMode


type PopupWindow
    = AdminPopup


type PopupStatus
    = PopupOpen PopupWindow
    | PopupClosed


type PhoneMode
    = PMShowDocument
    | PMShowDocumentList


type DocLoaded
    = NotLoaded
    | DocLoaded


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


type SortMode
    = SortAlphabetically
    | SortByMostRecent


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , url : Url.Url
    , message : String
    , statusReport : List String
    , inputSpecial : String
    , currentUser : Maybe Evergreen.V167.User.User
    , inputUsername : String
    , inputPassword : String
    , appMode : AppMode
    , windowWidth : Int
    , windowHeight : Int
    , popupStatus : PopupStatus
    , showEditor : Bool
    , authorId : String
    , phoneMode : PhoneMode
    , foundIds : List String
    , foundIdIndex : Int
    , selectedId : String
    , syncRequestIndex : Int
    , linenumber : Int
    , doSync : Bool
    , docLoaded : DocLoaded
    , documentsCreatedCounter : Int
    , initialText : String
    , sourceText : String
    , ast : Evergreen.V167.L0.SyntaxTree
    , editRecord : Evergreen.V167.Compiler.DifferentialParser.EditRecord (Tree.Tree Evergreen.V167.Parser.Block.IntermediateBlock) (Tree.Tree Evergreen.V167.Parser.Block.ExpressionBlock)
    , tableOfContents : List Evergreen.V167.Parser.Block.ExpressionBlock
    , title : List Evergreen.V167.Parser.Block.ExpressionBlock
    , searchCount : Int
    , searchSourceText : String
    , lineNumber : Int
    , permissions : DocPermissions
    , debounce : Debounce.Debounce String
    , currentDocument : Maybe Evergreen.V167.Document.Document
    , documents : List Evergreen.V167.Document.Document
    , inputSearchKey : String
    , printingState : PrintingState
    , documentDeleteState : DocumentDeleteState
    , counter : Int
    , publicDocuments : List Evergreen.V167.Document.Document
    , deleteDocumentState : DocumentDeleteState
    , sortMode : SortMode
    }


type alias DocumentDict =
    Dict.Dict String Evergreen.V167.Document.Document


type alias AuthorDict =
    Dict.Dict String String


type alias PublicIdDict =
    Dict.Dict String String


type alias AbstractDict =
    Dict.Dict String Evergreen.V167.Abstract.Abstract


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
    , authenticationDict : Evergreen.V167.Authentication.AuthenticationDict
    , documentDict : DocumentDict
    , authorIdDict : AuthorDict
    , publicIdDict : PublicIdDict
    , abstractDict : AbstractDict
    , usersDocumentsDict : UsersDocumentsDict
    , publicDocuments : List Evergreen.V167.Document.Document
    , documents : List Evergreen.V167.Document.Document
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
    | Home
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
    | SelectedText String
    | SyncLR
    | StartSync
    | NextSync
    | SendSyncLR
    | GetSelection String
    | SetInitialEditorContent
    | SetDocumentInPhoneAsCurrent DocPermissions Evergreen.V167.Document.Document
    | ShowTOCInPhone
    | InputSearchSource String
    | InputText String
    | DebounceMsg Debounce.Msg
    | Saved String
    | InputSearchKey String
    | Search
    | SearchText
    | InputAuthorId String
    | NewDocument
    | SetDocumentAsCurrent DocPermissions Evergreen.V167.Document.Document
    | SetPublic Evergreen.V167.Document.Document Bool
    | AskFoDocumentById String
    | AskForDocumentByAuthorId
    | DeleteDocument
    | SetDeleteDocumentState DocumentDeleteState
    | Render Evergreen.V167.Render.Msg.L0Msg
    | SetSortMode SortMode
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
    , authenticationDict : Evergreen.V167.Authentication.AuthenticationDict
    , documentDict : DocumentDict
    , authorIdDict : AuthorDict
    , publicIdDict : PublicIdDict
    , abstractDict : AbstractDict
    , usersDocumentsDict : UsersDocumentsDict
    , publicDocuments : List Evergreen.V167.Document.Document
    , documents : List Evergreen.V167.Document.Document
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
    | SaveDocument (Maybe Evergreen.V167.User.User) Evergreen.V167.Document.Document
    | GetDocumentByAuthorId String
    | GetDocumentByPublicId String
    | GetDocumentById String
    | CreateDocument (Maybe Evergreen.V167.User.User) Evergreen.V167.Document.Document
    | StealDocument Evergreen.V167.User.User String
    | SearchForDocuments (Maybe String) String
    | DeleteDocumentBE Evergreen.V167.Document.Document


type BackendMsg
    = NoOpBackendMsg
    | GotAtomsphericRandomNumber (Result Http.Error String)
    | Tick Time.Posix


type ToFrontend
    = NoOpToFrontend
    | SendBackupData String
    | SendUser Evergreen.V167.User.User
    | SendDocument DocPermissions Evergreen.V167.Document.Document
    | SendDocuments (List Evergreen.V167.Document.Document)
    | SendMessage String
    | StatusReport (List String)
    | SetShowEditor Bool
    | GotPublicDocuments (List Evergreen.V167.Document.Document)
