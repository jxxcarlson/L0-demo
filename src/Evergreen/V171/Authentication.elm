module Evergreen.V171.Authentication exposing (..)

import Dict
import Evergreen.V171.Credentials
import Evergreen.V171.User


type alias Username =
    String


type alias UserData =
    { user : Evergreen.V171.User.User
    , credentials : Evergreen.V171.Credentials.Credentials
    }


type alias AuthenticationDict =
    Dict.Dict Username UserData
