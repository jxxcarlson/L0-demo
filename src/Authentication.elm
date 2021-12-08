module Authentication exposing
    ( AuthenticationDict
    , UserData
    , encryptForTransit
    , insert
    , updateUser
    , users
    , verify
    )

import Config
import Credentials exposing (Credentials)
import Crypto.HMAC exposing (sha256)
import Dict exposing (Dict)
import Config
import User exposing (User)


type alias Username =
    String


type alias UserData =
    { user : User, credentials : Credentials }


type alias AuthenticationDict =
    Dict Username UserData


updateUser : User -> AuthenticationDict -> AuthenticationDict
updateUser user authDict =
    case Dict.get user.username authDict of
        Nothing ->
            authDict

        Just userData ->
            let
                newUserData =
                    { userData | user = user }
            in
            Dict.insert user.username newUserData authDict


users : AuthenticationDict -> List User
users authDict =
    authDict |> Dict.values |> List.map .user


insert : User -> String -> String -> AuthenticationDict -> Result String AuthenticationDict
insert user salt transitPassword authDict =
    case Credentials.hashPw salt transitPassword of
        Err _ ->
            Err "Could not generate credentials"

        Ok credentials ->
            Ok (Dict.insert user.username { user = user, credentials = credentials } authDict)


encryptForTransit : String -> String
encryptForTransit str =
    Crypto.HMAC.digest sha256 Config.transitKey str


verify : String -> String -> AuthenticationDict -> Bool
verify username transitPassword authDict =
    case Dict.get username authDict of
        Nothing ->
            False

        Just data ->
            case Credentials.check transitPassword data.credentials of
                Ok () ->
                    True

                Err _ ->
                    False
