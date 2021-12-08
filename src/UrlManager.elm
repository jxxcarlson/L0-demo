module UrlManager exposing (handleDocId)

import Lamdera exposing (sendToBackend)
import Parser exposing (..)
import Types exposing (FrontendMsg(..), ToBackend(..))
import Url exposing (Url)
import Url.Builder


type DocUrl
    = DocUrl String
    | HomePage String
    | NoDocUrl


handleDocId : Url -> Cmd FrontendMsg
handleDocId url =
    case parseDocUrl url of
        NoDocUrl ->
            Cmd.none

        HomePage _ ->
            Cmd.none

        DocUrl slug ->
            sendToBackend (GetDocumentByAuthorId slug)



-- PARSE


getInternalRef : String -> Maybe String
getInternalRef str =
    case run parseInternalRef str of
        Ok str_ ->
            Just str_

        Err _ ->
            Nothing


parseInternalRef : Parser String
parseInternalRef =
    succeed identity
        |. int
        |. symbol "#"
        |= parseRefString


parseRefString : Parser String
parseRefString =
    getChompedString <|
        chompWhile (\c -> Char.isAlphaNum c || c == '_')


parseDocUrl : Url -> DocUrl
parseDocUrl url =
    case run docUrlParser url.path of
        Ok docUrl ->
            docUrl

        Err _ ->
            NoDocUrl


docUrlParser : Parser DocUrl
docUrlParser =
    oneOf [ parseHomePage, docUrlUParser_ ]


docUrlUParser_ : Parser DocUrl
docUrlUParser_ =
    succeed (\k -> DocUrl k)
        |. symbol "/"
        |= oneOf [ uuidParser ]


uuidParser : Parser String
uuidParser =
    succeed identity
        |. symbol "uuid:"
        |= parseUuid



--


parseUuid : Parser String
parseUuid =
    getChompedString <|
        chompWhile (\c -> Char.isAlphaNum c || c == '-')


parseAlphaNum : Parser String
parseAlphaNum =
    getChompedString <|
        chompWhile (\c -> Char.isAlphaNum c)


parseHomePage : Parser DocUrl
parseHomePage =
    succeed HomePage
        |. symbol "/h/"
        |= parseAlphaNum
