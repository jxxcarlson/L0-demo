module DateTimeUtility exposing (toUtcSlug, toUtcString)

import List.Extra
import Time exposing (Month(..), toDay, toHour, toMinute, toMonth, toSecond, toYear, utc)


toUtcString : Time.Posix -> String
toUtcString time =
    String.fromInt (toYear utc time)
        ++ "-"
        ++ (toMonth utc time |> monthString)
        ++ "-"
        ++ String.fromInt (toDay utc time)
        ++ "-"
        ++ String.fromInt (toHour utc time)
        ++ ":"
        ++ String.fromInt (toMinute utc time)
        ++ ":"
        ++ String.fromInt (toSecond utc time)


toUtcSlug : Time.Posix -> String
toUtcSlug time =
    String.fromInt (toYear utc time)
        ++ "-"
        ++ (toMonth utc time |> monthString)
        ++ "-"
        ++ String.fromInt (toDay utc time)
        ++ "-"
        ++ (toHour utc time |> letter)
        ++ (toMinute utc time |> modBy 26 |> letter)
        ++ (toSecond utc time |> modBy 26 |> letter)


alphabet =
    String.split "" "abcdefghijklmnopqrstuvwxyz"


letter : Int -> String
letter k =
    List.Extra.getAt k alphabet |> Maybe.withDefault "0"



--++ ":"
--++ String.fromInt (toMinute utc time)
--++ ":"
--++ String.fromInt (toSecond utc time)


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
