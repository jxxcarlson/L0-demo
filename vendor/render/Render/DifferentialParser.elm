module Render.DifferentialParser exposing (EditRecord, differentialParser, init, update)

import Render.Differ as Differ


type alias EditRecord chunk parsedChunk =
    { chunks : List chunk
    , parsed : List parsedChunk
    }


init : (String -> List chunk) -> (chunk -> parsedChunk) -> String -> EditRecord chunk parsedChunk
init chunker parser text =
    let
        chunks =
            chunker text

        parsed =
            List.map parser chunks
    in
    { chunks = chunks, parsed = parsed }


{-| The update function takes an EditRecord and a string, the "text",
breaks the text into a list of logical paragraphs, diffs it with the list
of paragraphs held by the EditRecord, uses `differentialRender` to
render the changed paragraphs while copying the unchanged rendered paragraphsto
prodduce an updated list of rendered paragraphs. The 'differentialRender'
accomplishes this using the transformer. The seed is used to produces
a differential idList. This last step is perhaps unnecessary. To investigate.
(This was part of an optimization scheme.)
-}
update :
    (String -> List chunk)
    -> (chunk -> parsedChunk)
    -> EditRecord chunk parsedChunk
    -> String
    -> EditRecord chunk parsedChunk
update chunker parser editRecord text =
    let
        newChunks =
            chunker text

        diffRecord =
            Differ.diff editRecord.chunks newChunks

        parsed =
            differentialParser parser diffRecord editRecord
    in
    { chunks = newChunks, parsed = parsed }


differentialParser :
    (chunk -> parsedChunk)
    -> Differ.DiffRecord chunk
    -> EditRecord chunk parsedChunk
    -> List parsedChunk
differentialParser parser diffRecord editRecord =
    let
        ii =
            List.length diffRecord.commonInitialSegment

        it =
            List.length diffRecord.commonTerminalSegment

        initialSegmentParsed =
            List.take ii editRecord.parsed

        terminalSegmentParsed =
            takeLast it editRecord.parsed

        middleSegmentParsed =
            List.map parser diffRecord.middleSegmentInTarget
    in
    initialSegmentParsed ++ middleSegmentParsed ++ terminalSegmentParsed


takeLast : Int -> List a -> List a
takeLast k x =
    x |> List.reverse |> List.take k |> List.reverse
