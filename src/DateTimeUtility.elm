module DateTimeUtility exposing (toUtcSlug)

import List.Extra
import Time exposing (Month(..), toDay, toHour, toMinute, toMonth, toSecond, toYear, utc)


toUtcSlug : String -> String -> Time.Posix -> String
toUtcSlug str1 str2 time =
    String.fromInt (toYear utc time)
        ++ "-"
        ++ (toMonth utc time |> monthString)
        ++ "-"
        ++ String.fromInt (toDay utc time)
        ++ "-"
        ++ str1
        ++ (toHour utc time |> String.fromInt)
        ++ str2


hmsSlug : Int -> Int -> Int -> String
hmsSlug h m s =
    modBy 1000 (3600 * h + 60 * m + s) |> String.fromInt


alphabet =
    String.split "" "abcdefghijklmnopqrstuvwxyz"


letter : Int -> String
letter k =
    List.Extra.getAt k alphabet |> Maybe.withDefault "0"


monthString : Time.Month -> String
monthString month =
    case month of
        Jan ->
            "01"

        Feb ->
            "02"

        Mar ->
            "03"

        Apr ->
            "04"

        May ->
            "05"

        Jun ->
            "06"

        Jul ->
            "07"

        Aug ->
            "08"

        Sep ->
            "09"

        Oct ->
            "10"

        Nov ->
            "11"

        Dec ->
            "12"
