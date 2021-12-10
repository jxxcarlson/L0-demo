module Evergreen.V15.Authentication exposing (..)

import Dict
import Evergreen.V15.Credentials
import Evergreen.V15.User


type alias Username =
    String


type alias UserData =
    { user : Evergreen.V15.User.User
    , credentials : Evergreen.V15.Credentials.Credentials
    }


type alias AuthenticationDict =
    Dict.Dict Username UserData
