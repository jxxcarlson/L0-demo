module Evergreen.V168.Authentication exposing (..)

import Dict
import Evergreen.V168.Credentials
import Evergreen.V168.User


type alias Username =
    String


type alias UserData =
    { user : Evergreen.V168.User.User
    , credentials : Evergreen.V168.Credentials.Credentials
    }


type alias AuthenticationDict =
    Dict.Dict Username UserData
