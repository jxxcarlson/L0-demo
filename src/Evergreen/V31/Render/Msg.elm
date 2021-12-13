module Evergreen.V31.Render.Msg exposing (..)


type MarkupMsg
    = SendMeta
        { begin : Int
        , end : Int
        , index : Int
        }
    | SendLineNumber Int
    | GetPublicDocument String
