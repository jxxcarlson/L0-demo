module Evergreen.V71.Authentication exposing (..)

import Dict
import Evergreen.V71.Credentials
import Evergreen.V71.User


type alias Username =
    String


type alias UserData =
    { user : Evergreen.V71.User.User
    , credentials : Evergreen.V71.Credentials.Credentials
    }


type alias AuthenticationDict =
    Dict.Dict Username UserData
