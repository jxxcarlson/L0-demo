module Evergreen.V169.Compiler.DifferentialParser exposing (..)


type alias EditRecord chunk parsedChunk =
    { chunks : List chunk
    , parsed : List parsedChunk
    }
