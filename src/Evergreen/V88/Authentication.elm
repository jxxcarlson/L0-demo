module Evergreen.V88.Authentication exposing (..)

import Dict
import Evergreen.V88.Credentials
import Evergreen.V88.User


type alias Username =
    String


type alias UserData =
    { user : Evergreen.V88.User.User
    , credentials : Evergreen.V88.Credentials.Credentials
    }


type alias AuthenticationDict =
    Dict.Dict Username UserData
