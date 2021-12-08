module Frontend.PDF exposing (gotLink, print)

import Document exposing (Document)
import Http
import Json.Encode as E
import LaTeX.Export.API
import Process
import Task
import Types exposing (FrontendModel, FrontendMsg(..), PrintingState(..), ToBackend(..))


print model =
    case model.currentDocument of
        Nothing ->
            ( model, Cmd.none )

        Just doc ->
            ( { model | message = "printToPDF" }
            , Cmd.batch
                [ generatePdf doc
                , Process.sleep 1 |> Task.perform (always (ChangePrintingState PrintProcessing))
                ]
            )


generatePdf : Document -> Cmd FrontendMsg
generatePdf document =
    let
        data =
            LaTeX.Export.API.prepareForExportWithImages document.language document.content
    in
    Http.request
        { method = "POST"
        , headers = [ Http.header "Content-Type" "application/json" ]
        , url = "https://pdfserv.app/pdf"
        , body = Http.jsonBody (encodeForPDF document.id (normalizeTitle document.title) data.source data.imageUrls)
        , expect = Http.expectString GotPdfLink
        , timeout = Nothing
        , tracker = Nothing
        }


normalizeTitle : String -> String
normalizeTitle str =
    str
        |> String.toLower
        |> String.replace " " "-"


gotLink : FrontendModel -> Result error value -> ( FrontendModel, Cmd FrontendMsg )
gotLink model result =
    case result of
        Err _ ->
            ( model, Cmd.none )

        Ok docId ->
            ( model
            , Cmd.batch
                [ Process.sleep 5 |> Task.perform (always (ChangePrintingState PrintReady))
                ]
            )


encodeForPDF : String -> String -> String -> List String -> E.Value
encodeForPDF id title content urlList =
    E.object
        [ ( "id", E.string id )
        , ( "content", E.string content )
        , ( "urlList", E.list E.string urlList )
        ]
