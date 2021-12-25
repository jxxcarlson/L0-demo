module Evergreen.V65.Authentication exposing (..)

import Dict
import Evergreen.V65.Credentials
import Evergreen.V65.User


type alias Username =
    String


type alias UserData =
    { user : Evergreen.V65.User.User
    , credentials : Evergreen.V65.Credentials.Credentials
    }


type alias AuthenticationDict =
    Dict.Dict Username UserData
