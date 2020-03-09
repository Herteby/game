module Misc exposing (..)

import Html exposing (Attribute, Html, text)
import Html.Attributes exposing (classList)


none : Html msg
none =
    text ""


attrIf : Bool -> Attribute msg -> Attribute msg
attrIf bool attr =
    if bool then
        attr

    else
        classList []
