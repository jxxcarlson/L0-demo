module Document exposing
    ( Access(..)
    , Document
    , defaultSettings
    , empty
    , wordCount
    )

import Render.Settings
import Time
import User exposing (User)


type alias Document =
    { id : String
    , publicId : String
    , created : Time.Posix
    , modified : Time.Posix
    , content : String
    , title : String
    , public : Bool
    , author : Maybe String
    }


type alias Username =
    String


type Access
    = Public
    | Private
    | Shared { canRead : List Username, canWrite : List Username }


defaultSettings : Render.Settings.Settings
defaultSettings =
    { width = 500
    , titleSize = 30
    , paragraphSpacing = 28
    , showTOC = True
    , showErrorMessages = False
    , selectedId = ""
    }


empty =
    { id = "-3"
    , publicId = "-1"
    , created = Time.millisToPosix 0
    , modified = Time.millisToPosix 0
    , content = ""
    , title = "(Untitled)"
    , public = False
    , author = Nothing
    }


wordCount : Document -> Int
wordCount doc =
    doc.content |> String.words |> List.length
