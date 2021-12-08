module Evergreen.V1.Render.Msg exposing (..)


type MarkupMsg
    = SendMeta
        { begin : Int
        , end : Int
        , index : Int
        }
    | GetPublicDocument String
