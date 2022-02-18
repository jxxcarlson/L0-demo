module Evergreen.V117.Authentication exposing (..)

import Dict
import Evergreen.V117.Credentials
import Evergreen.V117.User


type alias Username =
    String


type alias UserData =
    { user : Evergreen.V117.User.User
    , credentials : Evergreen.V117.Credentials.Credentials
    }


type alias AuthenticationDict =
    Dict.Dict Username UserData
