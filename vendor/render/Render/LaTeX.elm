module Render.LaTeX exposing (export, exportExpr, rawExport)

import Dict exposing (Dict)
import Either exposing (Either(..))
import L0 exposing (SyntaxTree)
import List.Extra
import Parser.Block exposing (BlockType(..), ExpressionBlock(..))
import Parser.Expr exposing (Expr(..))
import Render.ASTTools as ASTTools
import Render.Lambda as Lambda
import Render.Settings exposing (Settings)
import Tree exposing (Tree)


export : Settings -> SyntaxTree -> String
export settings ast =
    preamble (ASTTools.extractTextFromSyntaxTreeByKey "title" ast)
        (ASTTools.extractTextFromSyntaxTreeByKey "author" ast)
        (ASTTools.extractTextFromSyntaxTreeByKey "date" ast)
        ++ "\n\n"
        ++ rawExport settings ast
        ++ "\n\n\\end{document}\n"


rawExport : Settings -> SyntaxTree -> String
rawExport settings ast =
    ast
        |> List.map (Tree.map (exportBlock settings))
        |> List.map unravel
        |> String.join "\n\n"


exportBlock : Settings -> ExpressionBlock -> String
exportBlock settings ((ExpressionBlock { blockType, name, content, children }) as block) =
    case blockType of
        Paragraph ->
            case content of
                Left str ->
                    str

                Right exprs_ ->
                    exportExprList settings exprs_

        OrdinaryBlock args ->
            case content of
                Left str ->
                    ""

                Right exprs_ ->
                    let
                        name_ =
                            name |> Maybe.withDefault "anon"
                    in
                    case Dict.get name_ blockDict of
                        Just f ->
                            f settings args (exportExprList settings exprs_)

                        Nothing ->
                            if name_ == "defs" then
                                renderDefs settings exprs_

                            else
                                environment name_ (exportExprList settings exprs_)

        VerbatimBlock _ ->
            case content of
                Left str ->
                    case name of
                        Nothing ->
                            environment "anon" str

                        Just name1 ->
                            if name1 == "$$" || name1 == "math" then
                                [ "$$", str, "$$" ] |> String.join "\n"

                            else
                                case Dict.get name1 blockNames of
                                    Nothing ->
                                        environment "anon" str

                                    Just name2 ->
                                        environment name2 str

                Right exprs_ ->
                    "???"


renderDefs settings exprs =
    "%% Macro definitions from L0 text:\n"
        ++ exportExprList settings exprs



-- DICIONARIES


verbatimExprDict =
    Dict.empty


blockNames : Dict String String
blockNames =
    Dict.fromList
        [ ( "code", "verbatim" )
        ]


functionDict : Dict String String
functionDict =
    Dict.fromList
        [ ( "italic", "textit" )
        , ( "i", "textit" )
        , ( "bold", "textbf" )
        , ( "b", "textbf" )
        , ( "image", "imagecenter" )
        ]


blockDict : Dict String (Settings -> List String -> String -> String)
blockDict =
    Dict.fromList
        [ ( "title", \sett args body -> "" )
        , ( "subtitle", \sett args body -> "" )
        , ( "author", \sett args body -> "" )
        , ( "date", \sett args body -> "" )
        , ( "heading", \sett args body -> heading args body )
        ]


heading : List String -> String -> String
heading args body =
    case secondArg args of
        "1" ->
            macro1 "section" body

        "2" ->
            macro1 "subsection" body

        "3" ->
            macro1 "subsubsection" body

        _ ->
            macro1 "subheading" body


firstArg : List String -> String
firstArg args =
    case List.head args of
        Nothing ->
            "Error: expecting something here"

        Just arg ->
            arg


secondArg : List String -> String
secondArg args =
    case List.Extra.getAt 1 args of
        Nothing ->
            "Error: expecting something here"

        Just arg ->
            arg


macro1 : String -> String -> String
macro1 name arg =
    if name == "math" then
        "$" ++ arg ++ "$"

    else if name == "group" then
        arg

    else
        case Dict.get name functionDict of
            Nothing ->
                "\\" ++ name ++ "{" ++ String.trimLeft arg ++ "}"

            Just realName ->
                "\\" ++ realName ++ "{" ++ String.trimLeft arg ++ "}"


exportExprList : Settings -> List Expr -> String
exportExprList settings exprs =
    List.map (exportExpr settings) exprs |> String.join ""


exportExpr : Settings -> Expr -> String
exportExpr settings expr =
    case expr of
        Expr name exps_ _ ->
            if name == "lambda" then
                case Lambda.extract expr of
                    Just lambda ->
                        Lambda.toString (exportExpr settings) lambda

                    Nothing ->
                        "Error extracting lambda"

            else
                macro1 name (List.map (exportExpr settings) exps_ |> String.join " ")

        Text str _ ->
            str

        Verbatim name body _ ->
            renderVerbatim name body

        Error err ->
            "error: " ++ err


renderVerbatim name body =
    case Dict.get name verbatimExprDict of
        Nothing ->
            macro1 name body

        Just macroName ->
            macro1 macroName body


{-| Comment on this!
-}
unravel : Tree String -> String
unravel tree =
    let
        children =
            Tree.children tree
    in
    if List.isEmpty children then
        Tree.label tree

    else
        Tree.label tree ++ ((List.map unravel children |> List.map indentString) |> String.join "\n")


indentString s =
    "  " ++ s



-- HELPERS


tagged name body =
    "\\" ++ name ++ "{" ++ body ++ "}"


environment name body =
    [ tagged "begin" name, body, tagged "end" name ] |> String.join "\n"



-- PREAMBLE


preamble : String -> String -> String -> String
preamble title author date =
    """
\\documentclass[11pt, oneside]{article}

%% Packages
\\usepackage{geometry}
\\geometry{letterpaper}
\\usepackage{changepage}   % for the adjustwidth environment
\\usepackage{graphicx}
\\usepackage{wrapfig}
\\graphicspath{ {images/} }
\\usepackage{amssymb}
\\usepackage{amsmath}
\\usepackage{amscd}
\\usepackage{hyperref}
\\hypersetup{
    colorlinks=true,
    linkcolor=blue,
    filecolor=magenta,
    urlcolor=blue,
}
\\usepackage{xcolor}
\\usepackage{soul}


%% Commands
\\newcommand{\\code}[1]{{\\tt #1}}
\\newcommand{\\ellie}[1]{\\href{#1}{Link to Ellie}}
% \\newcommand{\\image}[3]{\\includegraphics[width=3cm]{#1}}

\\newcommand{\\imagecenter}[1]{
   \\medskip
   \\begin{figure}
   \\centering
    \\includegraphics[width=12cm,height=12cm,keepaspectratio]{#1}
    \\vglue0pt
    \\end{figure}
    \\medskip
}

\\newcommand{\\imagefloatright}[3]{
    \\begin{wrapfigure}{R}{0.30\\textwidth}
    \\includegraphics[width=0.30\\textwidth]{#1}
    \\caption{#2}
    \\end{wrapfigure}
}

\\newcommand{\\imagefloatleft}[3]{
    \\begin{wrapfigure}{L}{0.3-\\textwidth}
    \\includegraphics[width=0.30\\textwidth]{#1}
    \\caption{#2}
    \\end{wrapfigure}
}

\\newcommand{\\italic}[1]{{\\sl #1}}
\\newcommand{\\strong}[1]{{\\bf #1}}
\\newcommand{\\subheading}[1]{{\\bf #1}\\par}
\\newcommand{\\xlink}[2]{\\href{{https://minilatex.lamdera.app/g/#1}}{#2}}
\\newcommand{\\red}[1]{\\textcolor{red}{#1}}
\\newcommand{\\blue}[1]{\\textcolor{blue}{#1}}
\\newcommand{\\violet}[1]{\\textcolor{violet}{#1}}
\\newcommand{\\remote}[1]{\\textcolor{red}{#1}}
\\newcommand{\\local}[1]{\\textcolor{blue}{#1}}
\\newcommand{\\highlight}[1]{\\hl{#1}}
\\newcommand{\\note}[2]{\\textcolor{blue}{#1}{\\hl{#1}}}
\\newcommand{\\strike}[1]{\\st{#1}}
\\newcommand{\\term}[1]{{\\sl #1}}
\\newtheorem{remark}{Remark}
\\newcommand{\\comment}[1]{}
\\newcommand{\\innertableofcontents}{}

%% Theorems
\\newtheorem{theorem}{Theorem}
\\newtheorem{axiom}{Axiom}
\\newtheorem{lemma}{Lemma}
\\newtheorem{proposition}{Proposition}
\\newtheorem{corollary}{Corollary}
\\newtheorem{definition}{Definition}
\\newtheorem{example}{Example}
\\newtheorem{exercise}{Exercise}
\\newtheorem{problem}{Problem}
\\newtheorem{exercises}{Exercises}
\\newcommand{\\bs}[1]{$\\backslash$#1}
\\newcommand{\\texarg}[1]{\\{#1\\}}

%% Environments
\\renewenvironment{quotation}
  {\\begin{adjustwidth}{2cm}{} \\footnotesize}
  {\\end{adjustwidth}}

\\def\\changemargin#1#2{\\list{}{\\rightmargin#2\\leftmargin#1}\\item[]}
\\let\\endchangemargin=\\endlist

\\renewenvironment{indent}
  {\\begin{adjustwidth}{0.75cm}{}}
  {\\end{adjustwidth}}


\\definecolor{mypink1}{rgb}{0.858, 0.188, 0.478}
\\definecolor{mypink2}{RGB}{219, 48, 122}

\\newcommand{\\fontRGB}[4]{
    \\definecolor{mycolor}{RGB}{#1, #2, #3}
    \\textcolor{mycolor}{#4}
    }

\\newcommand{\\highlightRGB}[4]{
    \\definecolor{mycolor}{RGB}{#1, #2, #3}
    \\sethlcolor{mycolor}
    \\hl{#4}
     \\sethlcolor{yellow}
    }

\\newcommand{\\gray}[2]{
\\definecolor{mygray}{gray}{#1}
\\textcolor{mygray}{#2}
}

\\newcommand{\\white}[1]{\\gray{1}[#1]}
\\newcommand{\\medgray}[1]{\\gray{0.5}[#1]}
\\newcommand{\\black}[1]{\\gray{0}[#1]}

% Spacing
\\parindent0pt
\\parskip5pt


\\begin{document}


\\title{""" ++ title ++ """}
\\author{""" ++ author ++ """}
\\date{""" ++ date ++ """}

\\maketitle

\\tableofcontents

"""
