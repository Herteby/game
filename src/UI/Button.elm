module UI.Button exposing
    ( Action(..)
    , Target(..)
    , danger
    , extraSmall
    , maybeAction
    , orange
    , primary
    , secondary
    , small
    , yellow
    )

import FontAwesome.Icon exposing (Icon)
import Html exposing (..)
import Html.Attributes as Attributes exposing (href, target)
import Html.Events exposing (onClick)
import UI exposing (..)
import UI.Icon as Icon


class : String -> Attribute msg
class =
    namespace "Button"


type alias Config msg =
    { action : Action msg
    , icon : Maybe Icon
    , text : String
    , attrs : List (Attribute msg)
    }


type Action msg
    = Enabled msg
    | Link String Target
    | Disabled
    | Spinner


type Target
    = Self
    | Blank
    | Parent
    | Top


{-| White text on blue background
-}
primary : Config msg -> Html msg
primary =
    btn "primary"


{-| Black text and blue icon on white background
-}
secondary : Config msg -> Html msg
secondary =
    btn "secondary"


{-| Black text and red icon on white background
-}
danger : Config msg -> Html msg
danger =
    btn "danger"


{-| White text on orange background
-}
orange : Config msg -> Html msg
orange =
    btn "orange"


{-| White text on yellow background
-}
yellow : Config msg -> Html msg
yellow =
    btn "yellow"


maybeAction : Maybe msg -> Action msg
maybeAction maybe =
    case maybe of
        Just msg ->
            Enabled msg

        Nothing ->
            Disabled


{-| Base button, not for use
-}
btn : String -> Config msg -> Html msg
btn kind { action, icon, text, attrs } =
    let
        ( element, attributes ) =
            case action of
                Enabled msg ->
                    ( button, [ onClick msg ] )

                Link url target_ ->
                    ( a
                    , [ href url
                      , target <|
                            case target_ of
                                Self ->
                                    "_self"

                                Blank ->
                                    "_blank"

                                Parent ->
                                    "_parent"

                                Top ->
                                    "_top"
                      ]
                    )

                Disabled ->
                    ( button, [ class "disabled" ] )

                Spinner ->
                    ( button, [ class "loading" ] )
    in
    element
        ([ class "button"
         , class kind
         , Attributes.tabindex -1
         ]
            ++ attributes
            ++ attrs
        )
        [ if action == Spinner then
            div [ class "spinner" ] [ Icon.spinner [] ]

          else
            none
        , case icon of
            Just icon_ ->
                div [ class "icon" ] [ Icon.view [] icon_ ]

            Nothing ->
                none
        , textSpan text
        ]


{-| Add this attribute to the button to make it smaller
-}
small : Attribute msg
small =
    class "small"


{-| Add this attribute to the button to make it extra small
-}
extraSmall : Attribute msg
extraSmall =
    class "extraSmall"
