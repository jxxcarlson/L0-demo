module Data exposing
    ( getDocumentByAuthorId
    , getDocumentByPublicId
    )

import Dict exposing (Dict)
import Document exposing (Document, empty)


documentDict : Dict String Document
documentDict =
    Dict.empty


{-| keys are privateIds of documents, values are document ids
-}
authorIdDict : Dict String String
authorIdDict =
    Dict.empty


{-| keys are publicIds of documents, values are document ids
-}
publicIdDict : Dict String String
publicIdDict =
    Dict.empty



--getDocumentByAuthorId : String -> Maybe Document
--getDocumentByAuthorId authorId =
--    Dict.get authorId authorIdDict |> Maybe.andThen (\k -> Dict.get k documentDict)


getDocumentByAuthorId : String -> Maybe Document
getDocumentByAuthorId authorId =
    let
        maybeId =
            Dict.get authorId authorIdDict

        maybeDoc =
            case maybeId of
                Nothing ->
                    Nothing

                Just id ->
                    Dict.get id documentDict
    in
    maybeDoc


getDocumentByPublicId : String -> Maybe Document
getDocumentByPublicId publicId =
    let
        id =
            Dict.get publicId publicIdDict
    in
    id |> Maybe.andThen (\k -> Dict.get k documentDict)



----XXXX----
