module Evergreen.V169.Authentication exposing (..)

import Dict
import Evergreen.V169.Credentials
import Evergreen.V169.User


type alias Username =
    String


type alias UserData =
    { user : Evergreen.V169.User.User
    , credentials : Evergreen.V169.Credentials.Credentials
    }


type alias AuthenticationDict =
    Dict.Dict Username UserData
