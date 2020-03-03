module Frontend exposing (Model, app)

import Css
import Dict
import GamePage
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Lamdera
import LoginPage
import Playground.Advanced as Playground
import RegisterPage
import Types exposing (..)


{-| Lamdera applications define 'app' instead of 'main'.

Lamdera.frontend is the same as Browser.application with the
additional update function; updateFromBackend.

-}
app =
    Lamdera.frontend
        { init = \_ _ -> init
        , update = update
        , updateFromBackend = updateFromBackend
        , view =
            \model ->
                { title = "Game"
                , body = [ view model ]
                }
        , subscriptions = subscriptions
        , onUrlChange = \_ -> Noop
        , onUrlRequest = \_ -> Noop
        }


subscriptions : Model -> Sub FrontendMsg
subscriptions model =
    Sub.map GameMsg Playground.subscriptions.all


type alias Model =
    FrontendModel


init : ( Model, Cmd FrontendMsg )
init =
    ( { page = StartPage }
    , Cmd.none
    )


{-| This is the normal frontend update function. It handles all messages that can occur on the frontend.
-}
update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case ( msg, model.page ) of
        ( LoginMsg submsg, LoginPage submodel ) ->
            LoginPage.update submsg submodel
                |> with LoginMsg LoginPage model

        ( RegisterMsg submsg, RegisterPage submodel ) ->
            RegisterPage.update submsg submodel
                |> with RegisterMsg RegisterPage model

        ( GameMsg submsg, GamePage submodel ) ->
            GamePage.game.update submsg submodel
                |> (\( newmodel, cmd ) ->
                        let
                            newMemory =
                                Playground.get newmodel |> Tuple.second

                            oldMemory =
                                Playground.get submodel |> Tuple.second

                            cmd_ =
                                if newMemory.player /= oldMemory.player then
                                    Lamdera.sendToBackend (UpdatePlayer newMemory.player)

                                else
                                    Cmd.none
                        in
                        ( { page = GamePage newmodel }
                        , Cmd.batch [ Cmd.map GameMsg cmd, cmd_ ]
                        )
                   )

        ( GotoLogin, _ ) ->
            ( { model | page = LoginPage LoginPage.init }, Cmd.none )

        ( GotoRegister, _ ) ->
            ( { model | page = RegisterPage RegisterPage.init }, Cmd.none )

        _ ->
            ( model, Cmd.none )


with msg page model =
    Tuple.mapBoth (\m -> { model | page = page m })
        (Cmd.map msg)


{-| This is the added update function. It handles all messages that can arrive from the backend.
-}
updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case ( msg, model.page ) of
        ( LoggedIn account, _ ) ->
            GamePage.init account
                |> with GameMsg GamePage model

        ( LoginFailed, LoginPage loginModel ) ->
            ( { page = LoginPage { loginModel | failed = True } }, Cmd.none )

        ( RegisterFailed, RegisterPage registerModel ) ->
            ( { page = RegisterPage { registerModel | failed = True } }, Cmd.none )

        ( UpdateOtherPlayer username character, GamePage game ) ->
            ( { model
                | page =
                    GamePage <|
                        Playground.edit
                            (\_ memory ->
                                { memory | others = Dict.insert username character memory.others }
                            )
                            game
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


view : Model -> Html FrontendMsg
view model =
    div [ class "main" ]
        [ Html.node "style" [] [ text Css.css ]
        , case model.page of
            LoginPage loginmodel ->
                LoginPage.view loginmodel |> Html.map LoginMsg

            RegisterPage regmodel ->
                RegisterPage.view regmodel |> Html.map RegisterMsg

            GamePage gamemodel ->
                GamePage.game.view gamemodel

            StartPage ->
                startView
        ]


startView =
    div [ class "startPage" ]
        [ button [ onClick GotoLogin ] [ text "Log in" ]
        , button [ onClick GotoRegister ] [ text "Register" ]
        ]
