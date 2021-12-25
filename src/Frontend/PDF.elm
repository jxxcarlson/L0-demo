module Frontend.PDF exposing (gotLink, print)

import Document exposing (Document)
import Either
import Http
import Json.Encode as E
import L0 exposing (SyntaxTree)
import Maybe.Extra
import Parser.Block exposing (ExpressionBlock(..))
import Process
import Render.ASTTools as ASTTools
import Render.LaTeX as LaTeX
import Render.Settings
import Task
import Tree
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
        syntaxTree =
            L0.parse document.content

        imageUrls : List String
        imageUrls =
            syntaxTree
                |> List.map Tree.flatten
                |> List.concat
                |> List.map (\(ExpressionBlock { content }) -> Either.toList content)
                |> List.concat
                |> List.concat
                |> ASTTools.filterExpressionsOnName "image"
                |> List.map (ASTTools.getText >> Maybe.map String.trim)
                |> Maybe.Extra.values

        contentForExport =
            LaTeX.export Render.Settings.defaultSettings syntaxTree
    in
    Http.request
        { method = "POST"
        , headers = [ Http.header "Content-Type" "application/json" ]
        , url = "https://pdfserv.app/pdf"
        , body = Http.jsonBody (encodeForPDF document.id "-" contentForExport imageUrls)
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
