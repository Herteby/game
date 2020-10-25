module LoginPage exposing (..)

import FontAwesome.Solid as Solid
import Hash
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Lamdera
import Types exposing (..)
import UI.Button as Button exposing (Action(..))


init : LoginModel
init =
    { username = ""
    , password = ""
    , failed = False
    }


update : LoginMsg -> LoginModel -> ( LoginModel, Cmd LoginMsg )
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


view : LoginModel -> Html LoginMsg
view model =
    div [ class "loginPage" ]
        [ Html.form [ class "form", onSubmit Submit ]
            [ label []
                [ text "Username"
                , input [ class "input", onInput LoginUsername, value model.username ] []
                ]
            , label []
                [ text "Password"
                , input
                    [ class "input"
                    , onInput LoginPassword
                    , value model.password
                    , type_ "password"
                    ]
                    []
                ]
            , if model.failed then
                text "Wrong username or password"

              else
                text ""
            , Button.primary
                { text = "Enter world"
                , action = Enabled Submit
                , icon = Just Solid.signIn
                , attrs = []
                }
            ]
        ]
