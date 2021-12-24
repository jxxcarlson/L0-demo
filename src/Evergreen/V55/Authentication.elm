module Evergreen.V55.Authentication exposing (..)

import Dict
import Evergreen.V55.Credentials
import Evergreen.V55.User


type alias Username =
    String


type alias UserData =
    { user : Evergreen.V55.User.User
    , credentials : Evergreen.V55.Credentials.Credentials
    }


type alias AuthenticationDict =
    Dict.Dict Username UserData
