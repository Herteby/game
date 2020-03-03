module RegisterPage exposing (..)

import Character
import Hash
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Lamdera
import Types exposing (..)


type alias Model =
    RegisterModel


type alias Msg =
    RegisterMsg


init : Model
init =
    { username = ""
    , password = ""
    , password2 = ""
    , character = Nothing
    }


update msg model =
    case msg of
        InputUsername string ->
            ( { model | username = string }, Cmd.none )

        InputPassword string ->
            ( { model | password = string }, Cmd.none )

        InputPassword2 string ->
            ( { model | password2 = string }, Cmd.none )

        SelectedCharacter int ->
            ( { model | character = Just int }, Cmd.none )

        Register ->
            case
                ( model.character |> Maybe.andThen Character.create
                , String.length model.password > 6 && model.password == model.password2
                )
            of
                ( Just character, True ) ->
                    ( model
                    , Lamdera.sendToBackend
                        (CreateAccount model.username
                            (Hash.fromString model.password)
                            character.variant
                        )
                    )

                _ ->
                    ( model, Cmd.none )


view model =
    div []
        [ Html.form [ class "form" ]
            [ label []
                [ text "Username"
                , input [ onInput InputUsername, value model.username ] []
                ]
            , label []
                [ text "Password"
                , input
                    [ onInput InputPassword
                    , value model.password
                    , type_ "password"
                    ]
                    []
                ]
            , label []
                [ text "Repeat password"
                , input
                    [ onInput InputPassword2
                    , value model.password2
                    , type_ "password"
                    ]
                    []
                ]
            ]
        , characterPicker model.character
        , button [ onClick Register ] [ text "Register" ]
        ]


characterPicker selected =
    div [ class "characterPicker" ]
        (h2 [] [ text "Choose a character" ]
            :: (List.range 1 40
                    |> List.map
                        (\variant ->
                            Html.button
                                [ classList [ ( "character", True ), ( "selected", selected == Just variant ) ]
                                , style "background-image" ("url(" ++ Character.url variant ++ ")")
                                , onClick (SelectedCharacter variant)
                                ]
                                []
                        )
               )
        )
