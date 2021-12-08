module Render.Settings exposing
    ( Settings
    , defaultSettings
    , leftIndentation
    , maxHeadingFontSize
    , redColor
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
    { width = 500
    , titleSize = 30
    , paragraphSpacing = 28
    , showTOC = True
    , showErrorMessages = False
    , selectedId = ""
    }


maxHeadingFontSize : Float
maxHeadingFontSize =
    32


leftIndentation =
    Element.paddingEach { left = 18, right = 0, top = 0, bottom = 0 }


redColor =
    Element.rgb 0.7 0 0
