module LoginPage exposing (..)

import Hash
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Lamdera
import Types exposing (..)


init : LoginModel
init =
    { username = ""
    , password = ""
    , failed = False
    }


update msg model =
    case msg of
        LoginUsername string ->
            ( { model | username = string, failed = False }, Cmd.none )

        LoginPassword string ->
            ( { model | password = string, failed = False }, Cmd.none )

        Submit ->
            ( model
            , Lamdera.sendToBackend (Login model.username (Hash.fromString model.password))
            )


view model =
    div [ class "loginPage" ]
        [ Html.form [ class "form", onSubmit Submit ]
            [ label []
                [ text "Username"
                , input [ onInput LoginUsername, value model.username ] []
                ]
            , label []
                [ text "Password"
                , input
                    [ onInput LoginPassword
                    , value model.password
                    , type_ "password"
                    ]
                    []
                ]
            , if model.failed then
                text "Wrong username or password"

              else
                text ""
            , button [] [ text "Log in" ]
            ]
        ]
