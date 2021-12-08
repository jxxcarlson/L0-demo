module Backend.Cmd exposing (exportJson, getRandomNumber)

import Backend.Backup
import Http
import Lamdera exposing (ClientId, SessionId, sendToFrontend)
import Types exposing (BackendModel, BackendMsg(..), ToFrontend(..))


getRandomNumber : Cmd BackendMsg
getRandomNumber =
    Http.get
        { url = randomNumberUrl 9
        , expect = Http.expectString GotAtomsphericRandomNumber
        }


{-| maxDigits < 10
-}
randomNumberUrl : Int -> String
randomNumberUrl maxDigits =
    let
        maxNumber =
            10 ^ maxDigits

        prefix =
            "https://www.random.org/integers/?num=1&min=1&max="

        suffix =
            "&col=1&base=10&format=plain&rnd=new"
    in
    prefix ++ String.fromInt maxNumber ++ suffix


exportJson : BackendModel -> ClientId -> Cmd msg
exportJson model clientId =
    sendToFrontend clientId (SendBackupData (Backend.Backup.encode model))



-- https://www.random.org/integers/?num=1&min=1&max=10000000000&col=1&base=10&format=plain&rnd=new
