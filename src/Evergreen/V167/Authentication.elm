module Evergreen.V167.Authentication exposing (..)

import Dict
import Evergreen.V167.Credentials
import Evergreen.V167.User


type alias Username =
    String


type alias UserData =
    { user : Evergreen.V167.User.User
    , credentials : Evergreen.V167.Credentials.Credentials
    }


type alias AuthenticationDict =
    Dict.Dict Username UserData
