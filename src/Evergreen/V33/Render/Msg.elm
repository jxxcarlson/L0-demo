module Evergreen.V33.Render.Msg exposing (..)


type MarkupMsg
    = SendMeta
        { begin : Int
        , end : Int
        , index : Int
        }
    | SendId String
    | GetPublicDocument String
