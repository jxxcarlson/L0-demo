module Frontend.Update exposing
    ( newDocument
    , updateCurrentDocument
    , updateWithViewport
    )

import Document exposing (Document)
import Lamdera exposing (sendToBackend)
import Lang.Lang
import List.Extra
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
            case model.language of
                Lang.Lang.L1 ->
                    "[title New Document]\n\n"

                Lang.Lang.Markdown ->
                    "[! title](New Document)\n\n"

                Lang.Lang.MiniLaTeX ->
                    "\\title{New Document}\n\n"

        doc =
            { emptyDoc
                | content = title
                , language = model.language
                , author = Maybe.map .username model.currentUser
            }
    in
    ( { model | showEditor = True }, sendToBackend (CreateDocument model.currentUser doc) )


updateCurrentDocument : Document -> FrontendModel -> FrontendModel
updateCurrentDocument doc model =
    { model | currentDocument = Just doc }
