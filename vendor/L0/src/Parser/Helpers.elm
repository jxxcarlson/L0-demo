module Parser.Helpers exposing
    ( Step(..)
    , loop
    , prependMessage
    )


type Step state a
    = Loop state
    | Done a


loop : state -> (state -> Step state a) -> a
loop s f =
    case f s of
        Loop s_ ->
            loop s_ f

        Done b ->
            b


prependMessage : String -> List String -> List String
prependMessage message messages =
    case messages of
        first :: rest ->
            if message == first then
                messages

            else
                message :: messages

        _ ->
            message :: messages
