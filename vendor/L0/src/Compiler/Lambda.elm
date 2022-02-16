module Compiler.Lambda exposing (Lambda, apply, expand, extract, insert, subst, toString)

import Dict exposing (Dict)
import Parser.Expr exposing (Expr(..))


type alias Lambda =
    { name : String, vars : List String, body : Expr }


extract : Expr -> Maybe Lambda
extract expr_ =
    case expr_ of
        Expr "lambda" ((Text argString _) :: expr :: []) _ ->
            case String.words argString of
                name :: rest ->
                    Just { name = name, vars = List.map (\x -> " " ++ x) rest, body = expr }

                _ ->
                    Nothing

        _ ->
            Nothing


{-| Insert a lambda in the dictionary
-}
insert : Maybe Lambda -> Dict String Lambda -> Dict String Lambda
insert data dict =
    case data of
        Nothing ->
            dict

        Just lambda ->
            Dict.insert lambda.name lambda dict


{-| Expand the given expression using the given dictionary of lambdas.
-}
expand : Dict String Lambda -> Expr -> Expr
expand dict expr =
    case expr of
        Expr name _ _ ->
            case Dict.get name dict of
                Nothing ->
                    expr

                Just lambda ->
                    apply lambda expr

        _ ->
            expr


{-| Substitute a for all occurrences of (Text var ..) in e
-}
subst : Expr -> String -> Expr -> Expr
subst a var body =
    case body of
        Text v meta ->
            if String.trim v == String.trim var then
                -- the trimming is a temporary hack.  Need to adjust the parser
                a

            else
                body

        Expr name exprs meta ->
            Expr name (List.map (subst a var) exprs) meta

        _ ->
            body


{-| Assume that var x is bound in a. For each expression e in exprs,
compute subst a x e. Let exprs2 be the resulting list. Return
E "group" exprs2 ...
-}
substInList : Expr -> String -> List Expr -> Expr
substInList a var exprs =
    Expr "group" (List.map (\e -> subst e var a) exprs) { begin = 0, end = 0, index = 0 }


{-| Apply a lambda to an expression.
-}
apply : Lambda -> Expr -> Expr
apply lambda expr =
    case List.head lambda.vars of
        Nothing ->
            -- Only handle one var lambdas for now
            expr

        Just var ->
            case expr of
                Expr fname_ exprs _ ->
                    if lambda.name == fname_ then
                        substInList lambda.body var exprs

                    else
                        expr

                _ ->
                    expr


toString : (Expr -> String) -> Lambda -> String
toString exprToString lambda =
    [ "\\newcommand{\\"
    , lambda.name
    , "}["
    , String.fromInt (List.length lambda.vars)
    , "]{"
    , lambda.body |> exprToString |> mapArgs lambda.vars
    , "}    "
    ]
        |> String.join ""


mapArgs : List String -> String -> String
mapArgs args str =
    List.foldl (\f acc -> f acc) str (List.indexedMap (\n arg -> mapArg n arg) args)


mapArg : Int -> String -> String -> String
mapArg n arg str =
    String.replace arg (String.fromInt n) str
