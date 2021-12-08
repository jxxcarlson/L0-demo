module Types exposing (..)

import Abstract exposing (Abstract, AbstractOLD)
import Authentication exposing (AuthenticationDict)
import Browser exposing (UrlRequest)
import Browser.Dom as Dom
import Browser.Navigation exposing (Key)
import Data
import Debounce exposing (Debounce)
import Dict exposing (Dict)
import Document exposing (Document)
import File exposing (File)
import Http
import Lang.Lang
import Markup.API exposing (ParseData)
import Random
import Render.Msg
import Time
import Url exposing (Url)
import User exposing (User)


type alias FrontendModel =
    { key : Key
    , url : Url
    , message : String

    -- ADMIN
    , statusReport : List String
    , inputSpecial : String

    -- USER
    , currentUser : Maybe User
    , inputUsername : String
    , inputPassword : String

    -- UI
    , appMode : AppMode
    , windowWidth : Int
    , windowHeight : Int
    , popupStatus : PopupStatus
    , showEditor : Bool
    , authorId : String

    -- SYNC
    , foundIds : List String
    , foundIdIndex : Int
    , selectedId : String
    , syncRequestIndex : Int

    -- DOCUMENT
    , parseData : ParseData
    , sourceText : String
    , searchCount : Int
    , searchSourceText : String
    , lineNumber : Int
    , permissions : DocPermissions
    , debounce : Debounce String
    , currentDocument : Maybe Document
    , documents : List Document
    , language : Lang.Lang.Lang
    , inputSearchKey : String
    , printingState : PrintingState
    , documentDeleteState : DocumentDeleteState
    , counter : Int
    , publicDocuments : List Document
    , deleteDocumentState : DocumentDeleteState
    }


type AppMode
    = UserMode
    | AdminMode


type PopupWindow
    = AdminPopup


type PopupStatus
    = PopupOpen PopupWindow
    | PopupClosed


type alias BackendModel =
    { message : String
    , currentTime : Time.Posix

    -- RANDOM
    , randomSeed : Random.Seed
    , uuidCount : Int
    , randomAtmosphericInt : Maybe Int

    -- USER
    , authenticationDict : AuthenticationDict

    -- DATA
    , documentDict : DocumentDict
    , authorIdDict : AuthorDict
    , publicIdDict : PublicIdDict
    , abstractDict : AbstractDict
    , usersDocumentsDict : UsersDocumentsDict
    , publicDocuments : List Document

    -- DOCUMENT
    , documents : List Document
    }


type alias DocumentLink =
    { digest : String, label : String, url : String }



-- Entries for the first three dictionaries are created when a document
-- is created.


{-| author's doc id -> docId; enables author access to doc via a link
-}
type alias AuthorDict =
    Dict String String


{-| public's doc id -> docId; enables public access to doc via a link
-}
type alias PublicIdDict =
    Dict String String


{-| docId -> Document
-}
type alias DocumentDict =
    Dict String Document


{-| User id -> List docId
-}
type alias UsersDocumentsDict =
    Dict UserId (List DocId)



-- The document abstract is updated every time a document is saved.


{-| docId -> Document abstracts
-}
type alias AbstractDict =
    Dict String Abstract


type alias AbstractDictOLD =
    Dict String AbstractOLD


type alias UserId =
    String


type alias DocId =
    String


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
      -- UI
    | SetAppMode AppMode
    | GotNewWindowDimensions Int Int
    | GotViewport Dom.Viewport
    | SetViewPortForElement (Result Dom.Error ( Dom.Element, Dom.Viewport ))
    | ChangePopupStatus PopupStatus
    | CloseEditor
    | OpenEditor
      -- ADMIN
    | InputSpecial String
    | RunSpecial
    | ExportJson
    | JsonRequested
    | JsonSelected File
    | JsonLoaded String
      -- USER
    | SignIn
    | SignOut
    | InputUsername String
    | InputPassword String
      -- SYNC
    | SyncLR
    | SendSyncLR
    | GetSelection String
      -- DOC
    | InputSearchSource String
    | Render Render.Msg.MarkupMsg
    | InputText String
    | DebounceMsg Debounce.Msg
    | Saved String
    | InputSearchKey String
    | Search
    | InputAuthorId String
    | NewDocument
    | SetDocumentAsCurrent DocPermissions Document
    | SetLanguage Lang.Lang.Lang
    | SetPublic Document Bool
    | AskFoDocumentById String
    | AskForDocumentByAuthorId
    | DeleteDocument
    | SetDeleteDocumentState DocumentDeleteState
      -- Export
    | ExportToMarkdown
    | ExportToLaTeX
    | Export
      -- PDF
    | PrintToPDF
    | GotPdfLink (Result Http.Error String)
    | ChangePrintingState PrintingState
    | FinallyDoCleanPrintArtefacts String
      ---
    | Help String


type PrintingState
    = PrintWaiting
    | PrintProcessing
    | PrintReady


type DocumentDeleteState
    = WaitingForDeleteAction
    | CanDelete


type SearchTerm
    = Query String


type ToBackend
    = NoOpToBackend
      -- ADMIN
    | GetBackupData
    | RunTask
    | GetStatus
    | RestoreBackup BackupOLD
      -- USER
    | SignInOrSignUp String String
      -- DOCUMENT
    | FetchDocumentById String
    | GetPublicDocuments
    | SaveDocument (Maybe User) Document
    | GetDocumentByAuthorId String
    | GetDocumentByPublicId String
    | CreateDocument (Maybe User) Document
    | StealDocument User String
    | SearchForDocuments (Maybe String) String
    | DeleteDocumentBE Document


type BackendMsg
    = NoOpBackendMsg
    | GotAtomsphericRandomNumber (Result Http.Error String)
    | Tick Time.Posix


type ToFrontend
    = NoOpToFrontend
      -- ADMIN
    | SendBackupData String
      -- USEr
    | SendUser User
      -- DOCUMENT
    | SendDocument DocPermissions Document
    | SendDocuments (List Document)
    | SendMessage String
    | StatusReport (List String)
    | SetShowEditor Bool
    | GotPublicDocuments (List Document)


type DocPermissions
    = ReadOnly
    | CanEdit


type alias BackupOLD =
    { message : String
    , currentTime : Time.Posix

    -- RANDOM
    , randomSeed : Random.Seed
    , uuidCount : Int
    , randomAtmosphericInt : Maybe Int

    -- USER
    , authenticationDict : Authentication.AuthenticationDict

    -- DATA
    , documentDict : DocumentDict
    , authorIdDict : AuthorDict
    , publicIdDict : PublicIdDict
    , abstractDict : AbstractDict
    , usersDocumentsDict : UsersDocumentsDict
    , publicDocuments : List Document

    --
    ---- DOCUMENTS
    , documents : List Document
    }
