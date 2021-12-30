module Evergreen.V71.Render.DifferentialParser exposing (..)


type alias EditRecord chunk parsedChunk =
    { chunks : List chunk
    , parsed : List parsedChunk
    }
