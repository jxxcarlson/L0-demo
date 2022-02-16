module Frontend.Update exposing
    ( newDocument
    , updateCurrentDocument
    , updateWithViewport
    )

import Document exposing (Document)
import Frontend.Cmd
import Lamdera exposing (sendToBackend)
import Types exposing (..)


updateWithViewport vp model =
    let
        w =
            round vp.viewport.width

        h =
            round vp.viewport.height
    in
    ( { model
        | windowWidth = w
        , windowHeight = h
      }
    , Cmd.none
    )


newDocument model =
    let
        emptyDoc =
            Document.empty

        title =
            "| title\nNew Document\n\n"

        doc =
            { emptyDoc
                | content = title
                , author = Maybe.map .username model.currentUser
            }
    in
    ( { model | showEditor = True }, Cmd.batch [ Frontend.Cmd.setInitialEditorContent, sendToBackend (CreateDocument model.currentUser doc) ] )


updateCurrentDocument : Document -> FrontendModel -> FrontendModel
updateCurrentDocument doc model =
    { model | currentDocument = Just doc }
