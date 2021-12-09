module Evergreen.V11.Authentication exposing (..)

import Dict
import Evergreen.V11.Credentials
import Evergreen.V11.User


type alias Username =
    String


type alias UserData =
    { user : Evergreen.V11.User.User
    , credentials : Evergreen.V11.Credentials.Credentials
    }


type alias AuthenticationDict =
    Dict.Dict Username UserData
