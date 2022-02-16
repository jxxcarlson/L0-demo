module Frontend.Cmd exposing (setInitialEditorContent, setupWindow)

import Backend.Backup
import Browser.Dom as Dom
import Document exposing (Document)
import Lamdera exposing (sendToBackend)
import Process
import Task
import Types exposing (FrontendModel, FrontendMsg(..), ToBackend(..))


setupWindow : Cmd FrontendMsg
setupWindow =
    Task.perform GotViewport Dom.getViewport


setInitialEditorContent : Cmd FrontendMsg
setInitialEditorContent =
    Process.sleep 50 |> Task.perform (always SetInitialEditorContent)
