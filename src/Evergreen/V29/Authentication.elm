module Evergreen.V29.Authentication exposing (..)

import Dict
import Evergreen.V29.Credentials
import Evergreen.V29.User


type alias Username =
    String


type alias UserData =
    { user : Evergreen.V29.User.User
    , credentials : Evergreen.V29.Credentials.Credentials
    }


type alias AuthenticationDict =
    Dict.Dict Username UserData
