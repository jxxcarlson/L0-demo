module Evergreen.V31.Authentication exposing (..)

import Dict
import Evergreen.V31.Credentials
import Evergreen.V31.User


type alias Username =
    String


type alias UserData =
    { user : Evergreen.V31.User.User
    , credentials : Evergreen.V31.Credentials.Credentials
    }


type alias AuthenticationDict =
    Dict.Dict Username UserData
