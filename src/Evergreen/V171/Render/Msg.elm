module Evergreen.V171.Render.Msg exposing (..)


type L0Msg
    = SendMeta
        { begin : Int
        , end : Int
        , index : Int
        }
    | SendId String
    | GetPublicDocument String
