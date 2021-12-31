module Parser.Simple exposing (ExprS(..), parse, simplify, simplifyToken, tokenize)

import Either exposing (Either)
import Parser.Advanced exposing (DeadEnd)
import Parser.Expr exposing (Expr(..))
import Parser.Expression as Expression exposing (State)
import Parser.Token as Token exposing (Token(..))
import Parser.Tools exposing (Context, Problem)


type ExprS
    = ExprS String (List ExprS)
    | TextS String
    | VerbatimS String String
    | EVS ExprS
    | ErrorS String


type alias StateS =
    { step : Int
    , tokens : List SimpleToken
    , tokenIndex : Int
    , committed : List ExprS
    , stack : List SimpleToken
    }


type SimpleToken
    = LBS
    | RBS
    | SS String
    | WS String
    | MathTokenS
    | CodeTokenS
    | TokenErrorS (List (DeadEnd Context Problem))


tokenize : String -> List SimpleToken
tokenize str =
    Token.run str |> List.map simplifyToken


parse : Int -> String -> StateS
parse k str =
    Expression.parseToState k str |> toStateS


toStateS : State -> StateS
toStateS state =
    { step = state.step
    , tokens = List.map simplifyToken state.tokens
    , committed = List.map simplify state.committed
    , tokenIndex = state.tokenIndex
    , stack = List.reverse <| simplifyStack state.stack
    }


simplifyToken : Token -> SimpleToken
simplifyToken token =
    case token of
        LB _ ->
            LBS

        RB _ ->
            RBS

        S str _ ->
            SS str

        W str _ ->
            WS str

        MathToken _ ->
            MathTokenS

        CodeToken _ ->
            CodeTokenS

        TokenError list _ ->
            TokenErrorS list


simplifyStack : List Token -> List SimpleToken
simplifyStack stack =
    List.map simplifyToken stack


simplify : Expr -> ExprS
simplify expr =
    case expr of
        Expr str expresssions _ ->
            ExprS str (List.map simplify expresssions)

        Text str _ ->
            TextS str

        Verbatim name str _ ->
            VerbatimS name str

        Error str ->
            ErrorS str



-- HELPERS
