module Parser.Expression exposing
    ( Expr(..)
    , State
    , parse
    , parse_
    , run
    )

import Either exposing (Either(..))
import List.Extra
import Parser.Match as M
import Parser.Symbol as Symbol exposing (Symbol(..))
import Parser.Token as Token exposing (Meta, Token(..), TokenType(..))



-- TYPES


type Expr
    = Expr String (List Expr) Meta
    | Text String Meta
    | Verbatim String String Meta
    | Error String


type alias State =
    { step : Int
    , tokens : List Token
    , numberOfTokens : Int
    , tokenIndex : Int
    , committed : List Expr
    , stack : List Token
    }



-- STATE FOR THE PARSER


init : String -> State
init str =
    let
        tokens =
            Token.run str |> List.reverse
    in
    { step = 0
    , tokens = tokens
    , numberOfTokens = List.length tokens
    , tokenIndex = 0
    , committed = []
    , stack = []
    }



-- PARSER


parse_ : String -> List Expr
parse_ str =
    parse str |> .committed


parse : String -> State
parse str =
    run (init str)


run : State -> State
run state =
    loop state nextStep
        |> (\state_ -> { state_ | committed = List.reverse state_.committed })


nextStep : State -> Step State State
nextStep state =
    case List.Extra.getAt state.tokenIndex state.tokens of
        Nothing ->
            if List.isEmpty state.stack then
                Done state

            else
                -- the stack is not empty, so we need to handle the parse error
                recoverFromError state

        Just token ->
            pushToken token { state | tokenIndex = state.tokenIndex + 1 }
                |> reduceState
                |> (\st -> { st | step = st.step + 1 })
                |> Loop



-- PUSH


pushToken : Token -> State -> State
pushToken token state =
    case token of
        S _ _ ->
            pushOrCommit token state

        W _ _ ->
            pushOrCommit token state

        MathToken _ ->
            pushOnStack token state

        CodeToken _ ->
            pushOnStack token state

        LB _ ->
            pushOnStack token state

        RB _ ->
            pushOnStack token state

        TokenError _ _ ->
            pushOnStack token state


pushOnStack : Token -> State -> State
pushOnStack token state =
    { state | stack = token :: state.stack }


pushOrCommit : Token -> State -> State
pushOrCommit token state =
    if List.isEmpty state.stack then
        commit token state

    else
        push token state


commit : Token -> State -> State
commit token state =
    case exprOfToken token of
        Nothing ->
            state

        Just expr ->
            { state | committed = expr :: state.committed }


exprOfToken : Token -> Maybe Expr
exprOfToken token =
    case token of
        S str loc ->
            Just (Text str loc)

        W str loc ->
            Just (Text str loc)

        _ ->
            Nothing


push : Token -> State -> State
push token state =
    { state | stack = token :: state.stack }



-- REDUCE


reduceState : State -> State
reduceState state =
    let
        symbols =
            state.stack |> Symbol.convertTokens |> List.reverse
    in
    if M.reducible symbols then
        case List.head symbols of
            Just L ->
                { state | stack = [], committed = eval (state.stack |> List.reverse) ++ state.committed }

            Just M ->
                { state | stack = [], committed = Verbatim "math" (Token.toString <| unbracket <| List.reverse state.stack) { begin = 0, end = 0, index = 0 } :: state.committed }

            Just C ->
                { state | stack = [], committed = Verbatim "code" (Token.toString <| unbracket <| List.reverse state.stack) { begin = 0, end = 0, index = 0 } :: state.committed }

            _ ->
                state

    else
        state


unbracket : List a -> List a
unbracket list =
    List.drop 1 (List.take (List.length list - 1) list)


areBracketed : List Token -> Bool
areBracketed tokens =
    List.map Token.type_ (List.take 1 tokens)
        == [ TLB ]
        && List.map Token.type_ (List.take 1 (List.reverse tokens))
        == [ TRB ]


eval : List Token -> List Expr
eval tokens =
    if areBracketed tokens then
        let
            args =
                unbracket tokens
        in
        case List.head args of
            Just (S name meta) ->
                [ Expr name (evalList (List.drop 1 args)) meta ]

            Nothing ->
                [ errorMessage "[ ]?" ]

            _ ->
                [ errorMessage <| "[" ++ Token.toString args ++ "]?" ]

    else
        []


evalList : List Token -> List Expr
evalList tokens =
    case List.head tokens of
        Just token ->
            case Token.type_ token of
                TLB ->
                    case M.match (Symbol.convertTokens2 tokens) of
                        Nothing ->
                            [ Text "error on match" dummyLoc ]

                        Just k ->
                            let
                                ( a, b ) =
                                    M.splitAt (k + 1) tokens
                            in
                            eval a ++ evalList b

                _ ->
                    case exprOfToken token of
                        Just expr ->
                            expr :: evalList (List.drop 1 tokens)

                        Nothing ->
                            [ Text "error converting Token" dummyLoc ]

        _ ->
            []


errorMessage : String -> Expr
errorMessage message =
    Expr "red" [ Text message dummyLoc ] dummyLoc


errorMessageBold : String -> Expr
errorMessageBold message =
    Expr "bold" [ Expr "red" [ Text message dummyLoc ] dummyLoc ] dummyLoc


errorMessage2 : String -> Expr
errorMessage2 message =
    Expr "blue" [ Text message dummyLoc ] dummyLoc


colorRed : Expr -> Expr
colorRed expr =
    Expr "red" [ expr ] dummyLoc


colorFirstElementRed : State -> State
colorFirstElementRed state =
    let
        ( a, b ) =
            M.splitAt 1 state.committed

        newCommitted =
            List.map colorRed a ++ b
    in
    { state | committed = newCommitted }


addErrorMessage : String -> State -> State
addErrorMessage message state =
    let
        committed =
            errorMessage message :: state.committed
    in
    { state | committed = committed }


isReducible : List Token -> Bool
isReducible tokens =
    tokens |> List.reverse |> Symbol.convertTokens |> M.reducible


recoverFromError : State -> Step State State
recoverFromError state =
    case List.reverse state.stack of
        (LB _) :: (RB meta) :: rest ->
            Loop
                { state
                    | committed = errorMessage "[?]" :: state.committed
                    , stack = []
                    , tokenIndex = meta.index + 1
                }

        (LB _) :: (LB meta) :: rest ->
            Loop
                { state
                    | committed = errorMessage "[" :: state.committed
                    , stack = []
                    , tokenIndex = meta.index
                }

        (LB _) :: (S fName meta) :: rest ->
            Loop
                { state
                    | committed = errorMessage ("[" ++ fName ++ errorSuffix rest) :: state.committed
                    , stack = []
                    , tokenIndex = meta.index + 1
                }

        (LB _) :: [] ->
            Done
                { state
                    | committed = errorMessage "[...?" :: state.committed
                    , stack = []
                    , tokenIndex = 0
                    , numberOfTokens = 0
                }

        (MathToken meta) :: rest ->
            let
                content =
                    Token.toString rest

                message =
                    if content == "" then
                        "$?$"

                    else
                        "$ "
            in
            Loop
                { state
                    | committed = errorMessage message :: state.committed
                    , stack = []
                    , tokenIndex = meta.index + 1
                    , numberOfTokens = 0
                }

        (CodeToken meta) :: rest ->
            let
                content =
                    Token.toString rest

                message =
                    if content == "" then
                        "`?`"

                    else
                        "` "
            in
            Loop
                { state
                    | committed = errorMessageBold message :: state.committed
                    , stack = []
                    , tokenIndex = meta.index + 1
                    , numberOfTokens = 0
                }

        _ ->
            recoverFromError1 state


errorSuffix rest =
    case rest of
        [] ->
            "]?"

        (W _ _) :: [] ->
            "]?"

        _ ->
            ""


recoverFromError1 : State -> Step State State
recoverFromError1 state =
    let
        k =
            Symbol.balance <| Symbol.convertTokens (List.reverse state.stack)

        newStack =
            List.repeat k (RB dummyLoc) ++ state.stack

        newSymbols =
            Symbol.convertTokens (List.reverse newStack)

        reducible =
            M.reducible newSymbols
    in
    if reducible then
        Done <|
            addErrorMessage " ]? " <|
                reduceState <|
                    { state
                        | stack = newStack
                        , tokenIndex = 0
                        , numberOfTokens = List.length newStack
                        , committed = errorMessage "[" :: state.committed
                    }

    else
        Done
            { state
                | committed =
                    braceError k
                        -- :: Expr "blue" [ Text (" " ++ Token.toString state.tokens) dummyLoc ] dummyLoc
                        :: state.committed
            }


braceError : Int -> Expr
braceError k =
    if k < 0 then
        let
            braces =
                List.repeat -k "]" |> String.join ""
        in
        errorMessage2 <| " " ++ braces ++ " << Too many right braces (" ++ String.fromInt -k ++ ")"

    else
        let
            braces =
                List.repeat k "[" |> String.join ""
        in
        errorMessage2 <| " " ++ braces ++ " << Too many left braces (" ++ String.fromInt k ++ ")"



-- HELPERS


dummyTokenIndex =
    0


dummyLoc =
    { begin = 0, end = 0, index = dummyTokenIndex }



-- LOOP


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
