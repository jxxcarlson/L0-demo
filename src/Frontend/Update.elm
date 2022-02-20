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

        documentsCreatedCounter =
            model.documentsCreatedCounter + 1

        titleString =
            "| title\nNew Document (" ++ String.fromInt documentsCreatedCounter ++ ")\n\n"

        doc =
            { emptyDoc
                | content = titleString
                , author = Maybe.map .username model.currentUser
            }
    in
    ( { model | showEditor = True, documentsCreatedCounter = documentsCreatedCounter }
    , Cmd.batch [ sendToBackend (CreateDocument model.currentUser doc) ]
    )


updateCurrentDocument : Document -> FrontendModel -> FrontendModel
updateCurrentDocument doc model =
    { model | currentDocument = Just doc }
