module UI exposing (attrIf, grid, highlight, interactive, linedGrid, modal, namespace, none, px, pxInt, showMore, spacer, textSpan)

import FontAwesome.Solid as Solid
import Html exposing (..)
import Html.Attributes as Attributes exposing (..)
import Html.Events exposing (onClick, stopPropagationOn)
import Json.Decode as Decode
import List.Extra as List
import Maybe.Extra as Maybe
import Regex
import UI.Icon as Icon


class =
    namespace "UI"


{-| Set up a CSS namespace for the module.

  - Should be used like: class = namespace "NameOfModule"
  - And create a NameOfModule.styl file starting with +prefix-classes('NameOfModule\_\_')

-}
namespace : String -> String -> Attribute msg
namespace prefix classNames =
    classNames
        |> String.split " "
        |> List.map (\str -> prefix ++ "__" ++ str)
        |> String.join " "
        |> Attributes.class


none : Html msg
none =
    text ""


attrIf : Bool -> Attribute msg -> Attribute msg
attrIf bool attr =
    if bool then
        attr

    else
        classList []


{-| invisible element with flex-grow: 1, pushes the elements after it down or to the right
-}
spacer : Html msg
spacer =
    div [ class "spacer" ] []


{-| This should usually used instead of `text`, to get correct margins in flex-layouts
-}
textSpan : String -> Html msg
textSpan string =
    if string == "" then
        none

    else
        span [] [ text string ]


px : Float -> String
px float =
    String.fromFloat float ++ "px"


pxInt : Int -> String
pxInt int =
    String.fromInt int ++ "px"


grid : List (List (Html msg)) -> Html msg
grid =
    grid_ "grid"


linedGrid : List (List (Html msg)) -> Html msg
linedGrid =
    grid_ "grid lined"


grid_ : String -> List (List (Html msg)) -> Html msg
grid_ class_ rows =
    let
        lastRowIndex =
            List.length rows - 1
    in
    case rows |> List.map List.length |> List.maximum of
        Just colCount ->
            rows
                |> List.indexedMap
                    (\i columns ->
                        columns
                            ++ List.repeat (colCount - List.length columns) none
                            |> List.map (\col -> div [ attrIf (i == lastRowIndex) (class "last") ] [ col ])
                    )
                |> List.concat
                |> div
                    [ class class_
                    , style "grid-template-columns" (String.repeat colCount "auto ")
                    ]

        Nothing ->
            none


showMore : Html msg -> Html msg -> Html msg
showMore summary details =
    Html.details [ class "showMore" ]
        [ Html.summary [] [ Icon.view [ class "caret" ] Solid.caretRight, summary ]
        , details
        ]


{-| Wrap all matching substrings with the provided Attribute (the Attribute should usually be a CSS class)
-}
highlight : Attribute msg -> String -> String -> Html msg
highlight attribute substring string =
    let
        regex =
            Regex.fromStringWith { caseInsensitive = True, multiline = False } substring
                |> Maybe.withDefault Regex.never

        matches =
            Regex.find regex string |> List.map (\match -> span [ attribute ] [ text match.match ])

        rest =
            Regex.split regex string |> List.map text
    in
    span [] (List.interweave rest matches)


modal : Maybe msg -> List (Html msg) -> Html msg
modal close content =
    div [ class "overlay" ]
        [ div [ class "modalCloser", Maybe.unwrap (classList []) onClick close ] []
        , div [ class "modal" ]
            content
        ]


interactive : Attribute msg
interactive =
    class "interactive"
