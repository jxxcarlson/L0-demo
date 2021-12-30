module Parser.Match exposing (deleteAt, match, reducible, splitAt)

import Parser.Helpers exposing (Step(..), loop)
import Parser.Symbol as Symbol exposing (Symbol(..), value)


reducible : List Symbol -> Bool
reducible symbols =
    case List.head symbols of
        Just M ->
            List.head (List.reverse (List.drop 1 symbols)) == Just M

        Just C ->
            List.head (List.reverse (List.drop 1 symbols)) == Just C

        _ ->
            reducibleF symbols


reducibleF : List Symbol -> Bool
reducibleF symbols =
    case List.head symbols of
        Nothing ->
            True

        Just R ->
            False

        Just O ->
            False

        Just M ->
            False

        Just C ->
            False

        Just L ->
            case match symbols of
                Nothing ->
                    False

                Just k ->
                    reducibleF (List.drop 1 (deleteAt k symbols))


{-|

> deleteAt 1 [0, 1, 2]

     [0,2] : List number

-}
deleteAt : Int -> List a -> List a
deleteAt k list =
    List.take k list ++ List.drop (k + 1) list


{-|

    > splitAt 2 [0, 1, 2, 3, 4]
      ([0,1],[3,4])

-}
splitAt : Int -> List a -> ( List a, List a )
splitAt k list =
    ( List.take k list, List.drop (k + 0) list )


type alias State =
    { symbols : List Symbol, index : Int, brackets : Int }


match : List Symbol -> Maybe Int
match symbols =
    case List.head symbols of
        Nothing ->
            Nothing

        Just symbol ->
            if value symbol < 0 then
                Nothing

            else
                loop { symbols = List.drop 1 symbols, index = 1, brackets = value symbol } nextStep


nextStep : State -> Step State (Maybe Int)
nextStep state =
    case List.head state.symbols of
        Nothing ->
            Done Nothing

        Just sym ->
            let
                brackets =
                    state.brackets + value sym
            in
            if brackets < 0 then
                Done Nothing

            else if brackets == 0 then
                Done (Just state.index)

            else
                Loop { symbols = List.drop 1 state.symbols, index = state.index + 1, brackets = brackets }
