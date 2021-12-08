module View.Utility exposing
    ( cssNode
    , elementAttribute
    , getElementWithViewPort
    , hideIf
    , katexCSS
    , noFocus
    , onEnter
    , setViewPortForSelectedLine
    , setViewPortToTop
    , setViewportForElement
    , showIf
    )

import Browser.Dom as Dom
import Element exposing (Element)
import Html
import Html.Attributes as HA
import Html.Events exposing (keyCode, on, onClick, onInput)
import Json.Decode as D
import Task exposing (Task)
import Types exposing (FrontendModel, FrontendMsg)


onEnter : FrontendMsg -> Html.Attribute FrontendMsg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                D.succeed msg

            else
                D.fail "not ENTER"
    in
    on "keydown" (keyCode |> D.andThen isEnter)


showIf : Bool -> Element msg -> Element msg
showIf isVisible element =
    if isVisible then
        element

    else
        Element.none


hideIf : Bool -> Element msg -> Element msg
hideIf condition element =
    if condition then
        Element.none

    else
        element


setViewportForElement : String -> Cmd FrontendMsg
setViewportForElement id =
    Dom.getViewportOf "__RENDERED_TEXT__"
        |> Task.andThen (\vp -> getElementWithViewPort vp id)
        |> Task.attempt Types.SetViewPortForElement


setViewPortToTop : Cmd FrontendMsg
setViewPortToTop =
    Task.attempt (\_ -> Types.NoOpFrontendMsg) (Dom.setViewportOf "__RENDERED_TEXT__" 0 0)


setViewPortForSelectedLine : Dom.Element -> Dom.Viewport -> Cmd FrontendMsg
setViewPortForSelectedLine element viewport =
    let
        y =
            -- viewport.viewport.y + element.element.y - element.element.height - 100
            viewport.viewport.y + element.element.y - element.element.height - 380
    in
    Task.attempt (\_ -> Types.NoOpFrontendMsg) (Dom.setViewportOf "__RENDERED_TEXT__" 0 y)


getElementWithViewPort : Dom.Viewport -> String -> Task Dom.Error ( Dom.Element, Dom.Viewport )
getElementWithViewPort vp id =
    Dom.getElement id
        |> Task.map (\el -> ( el, vp ))


noFocus : Element.FocusStyle
noFocus =
    { borderColor = Nothing
    , backgroundColor = Nothing
    , shadow = Nothing
    }


cssNode : String -> Element FrontendMsg
cssNode fileName =
    Html.node "link" [ HA.rel "stylesheet", HA.href fileName ] [] |> Element.html



-- Include KaTeX CSS


katexCSS : Element FrontendMsg
katexCSS =
    Element.html <|
        Html.node "link"
            [ HA.attribute "rel" "stylesheet"
            , HA.attribute "href" "https://cdn.jsdelivr.net/npm/katex@0.15.1/dist/katex.min.css"
            ]
            []


elementAttribute : String -> String -> Element.Attribute msg
elementAttribute key value =
    Element.htmlAttribute (HA.attribute key value)
