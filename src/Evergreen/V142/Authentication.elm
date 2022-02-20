module Evergreen.V142.Authentication exposing (..)

import Dict
import Evergreen.V142.Credentials
import Evergreen.V142.User


type alias Username =
    String


type alias UserData =
    { user : Evergreen.V142.User.User
    , credentials : Evergreen.V142.Credentials.Credentials
    }


type alias AuthenticationDict =
    Dict.Dict Username UserData
