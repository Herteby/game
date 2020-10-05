module UI.Icon exposing (masked, spinner, styled, view)

import FontAwesome.Attributes as Attributes
import FontAwesome.Icon as FA exposing (Icon)
import FontAwesome.Solid as Solid
import FontAwesome.Transforms exposing (Transform)
import Html exposing (..)
import Svg
import Svg.Attributes



{- This module wraps the FontAwesome package, making it easier to use by allowing HTML attributes instead of SVG attributes -}


{-| Display a FontAwesome Icon
-}
view : List (Attribute msg) -> Icon -> Html msg
view htmlAttrs icon =
    span htmlAttrs [ FA.viewStyled [ Svg.Attributes.class "fa-fw" ] icon ]


styled : List (Attribute msg) -> List (Svg.Attribute msg) -> Icon -> Html msg
styled htmlAttrs iconAttrs icon =
    span htmlAttrs [ FA.viewStyled (Svg.Attributes.class "fa-fw" :: iconAttrs) icon ]


masked : List (Attribute msg) -> List Transform -> Icon -> Icon -> Html msg
masked htmlAttrs transforms outerIcon innerIcon =
    span htmlAttrs
        [ innerIcon
            |> FA.present
            |> FA.withId outerIcon.name
            |> FA.transform transforms
            |> FA.masked outerIcon
            |> FA.styled [ Svg.Attributes.class "fa-fw" ]
            |> FA.view
        ]


{-| Animated loading spinner
-}
spinner : List (Attribute msg) -> Html msg
spinner attrs =
    styled attrs [ Attributes.pulse ] Solid.spinner
