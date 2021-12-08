module Frontend.Cmd exposing (setupWindow)

import Backend.Backup
import Browser.Dom as Dom
import Document exposing (Document)
import Lamdera exposing (sendToBackend)
import Task
import Types exposing (FrontendModel, FrontendMsg(..), ToBackend(..))


setupWindow : Cmd FrontendMsg
setupWindow =
    Task.perform GotViewport Dom.getViewport
