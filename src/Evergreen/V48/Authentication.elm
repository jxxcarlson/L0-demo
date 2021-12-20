module Evergreen.V48.Authentication exposing (..)

import Dict
import Evergreen.V48.Credentials
import Evergreen.V48.User


type alias Username =
    String


type alias UserData =
    { user : Evergreen.V48.User.User
    , credentials : Evergreen.V48.Credentials.Credentials
    }


type alias AuthenticationDict =
    Dict.Dict Username UserData
