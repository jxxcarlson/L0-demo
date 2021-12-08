module Backend.Update exposing
    ( deleteDocument
    , getUserDocuments
    , gotAtmosphericRandomNumber
    , setupUser
    , updateAbstracts
    )

import Abstract
import Authentication
import Dict
import Document
import Hex
import Lamdera exposing (ClientId, SessionId, broadcast, sendToFrontend)
import Maybe.Extra
import Random
import Token
import Types exposing (..)
import User exposing (User)


type alias Model =
    BackendModel



-- SYSTEM


deleteDocument : Document.Document -> Model -> ( Model, Cmd msg )
deleteDocument doc model =
    let
        documentDict =
            Dict.remove doc.id model.documentDict

        publicIdDict =
            Dict.remove doc.id model.publicIdDict

        abstractDict =
            Dict.remove doc.id model.abstractDict

        usersDocumentsDict =
            Dict.remove doc.id model.usersDocumentsDict

        authorIdDict =
            Dict.remove doc.id model.authorIdDict

        publicDocuments =
            List.filter (\d -> d.id /= doc.id) model.publicDocuments

        documents =
            List.filter (\d -> d.id /= doc.id) model.documents
    in
    ( { model
        | documentDict = documentDict
        , authorIdDict = authorIdDict
        , publicIdDict = publicIdDict
        , abstractDict = abstractDict
        , usersDocumentsDict = usersDocumentsDict
        , publicDocuments = publicDocuments
        , documents = documents
      }
    , Cmd.none
    )


gotAtmosphericRandomNumber : Model -> Result error String -> ( Model, Cmd msg )
gotAtmosphericRandomNumber model result =
    case result of
        Ok str ->
            case String.toInt (String.trim str) of
                Nothing ->
                    ( model, broadcast (SendMessage "Could not get atomospheric integer") )

                Just rn ->
                    let
                        newRandomSeed =
                            Random.initialSeed rn
                    in
                    ( { model
                        | randomAtmosphericInt = Just rn
                        , randomSeed = newRandomSeed
                      }
                    , broadcast (SendMessage ("Got atmospheric integer " ++ String.fromInt rn))
                    )

        Err _ ->
            ( model, Cmd.none )



-- USER


setupUser : Model -> ClientId -> String -> String -> ( BackendModel, Cmd BackendMsg )
setupUser model clientId username transitPassword =
    let
        ( randInt, seed ) =
            Random.step (Random.int (Random.minInt // 2) (Random.maxInt - 1000)) model.randomSeed

        randomHex =
            Hex.toString randInt |> String.toUpper

        tokenData =
            Token.get seed

        user =
            { username = username
            , id = tokenData.token
            , realname = "Undefined"
            , email = "Undefined"
            , created = model.currentTime
            , modified = model.currentTime
            }
    in
    case Authentication.insert user randomHex transitPassword model.authenticationDict of
        Err str ->
            ( { model | randomSeed = tokenData.seed }, sendToFrontend clientId (SendMessage ("Error: " ++ str)) )

        Ok authDict ->
            ( { model | randomSeed = tokenData.seed, authenticationDict = authDict, usersDocumentsDict = Dict.insert user.id [] model.usersDocumentsDict }
            , Cmd.batch
                [ sendToFrontend clientId (SendMessage "Success! You have set up your account")
                , sendToFrontend clientId (SendUser user)
                ]
            )


getUserDocuments : User -> UsersDocumentsDict -> DocumentDict -> List Document.Document
getUserDocuments user usersDocumentsDict documentDict =
    case Dict.get user.id usersDocumentsDict of
        Nothing ->
            []

        Just docIds ->
            List.foldl (\id acc -> Dict.get id documentDict :: acc) [] docIds |> Maybe.Extra.values


updateAbstract : Document.Document -> AbstractDict -> AbstractDict
updateAbstract doc dict =
    Dict.insert doc.id (Abstract.get doc.language doc.content) dict


updateAbstractById : String -> DocumentDict -> AbstractDict -> AbstractDict
updateAbstractById id docDict abstractDict =
    case Dict.get id docDict of
        Nothing ->
            abstractDict

        Just doc ->
            updateAbstract doc abstractDict


updateAbstracts : DocumentDict -> AbstractDict -> AbstractDict
updateAbstracts documentDict abstractDict =
    List.foldl (\id acc -> updateAbstractById id documentDict acc) abstractDict (Dict.keys documentDict)
