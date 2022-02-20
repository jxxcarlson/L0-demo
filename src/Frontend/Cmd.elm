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


setInitialEditorContent : Float -> Cmd FrontendMsg
setInitialEditorContent delay =
    Process.sleep delay |> Task.perform (always SetInitialEditorContent)
