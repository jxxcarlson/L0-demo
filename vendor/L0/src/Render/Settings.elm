module Render.Settings exposing
    ( Settings
    , blueColor
    , defaultSettings
    , leftIndent
    , leftIndentation
    , makeSettings
    , maxHeadingFontSize
    , redColor
    , topMarginForChildren
    , windowWidthScale
    )

import Element


type alias Settings =
    { paragraphSpacing : Int
    , selectedId : String
    , showErrorMessages : Bool
    , showTOC : Bool
    , titleSize : Int
    , width : Int
    }


defaultSettings : Settings
defaultSettings =
    makeSettings 1 600


makeSettings : Float -> Int -> Settings
makeSettings scale width =
    { width = round (scale * toFloat width)
    , titleSize = 30
    , paragraphSpacing = 28
    , showTOC = True
    , showErrorMessages = False
    , selectedId = ""
    }


windowWidthScale =
    0.3


maxHeadingFontSize : Float
maxHeadingFontSize =
    32


leftIndent =
    18


topMarginForChildren =
    6


leftIndentation =
    Element.paddingEach { left = 18, right = 0, top = 0, bottom = 0 }


redColor =
    Element.rgb 0.7 0 0


blueColor =
    Element.rgb 0 0 0.9
