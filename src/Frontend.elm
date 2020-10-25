module Frontend exposing (app, init)

import AltMath.Vector2 exposing (Vec2)
import Character
import Dict exposing (Dict)
import Env
import FontAwesome.Solid as Solid
import FontAwesome.Styles
import GamePage
import Hash
import Html exposing (..)
import Html.Attributes exposing (..)
import Lamdera
import LoginPage
import Maybe.Extra as Maybe
import Process
import RegisterPage
import Task
import Time
import Types exposing (..)
import UI.Button as Button exposing (Action(..))


{-| Lamdera applications define 'app' instead of 'main'.

Lamdera.frontend is the same as Browser.application with the
additional update function; updateFromBackend.

-}
app =
    Lamdera.frontend
        { init =
            \_ _ ->
                case Env.mode of
                    Env.Development ->
                        devInit

                    Env.Production ->
                        init
        , update = update
        , updateFromBackend = updateFromBackend
        , view =
            \model ->
                { title = "Game"
                , body =
                    [ FontAwesome.Styles.css
                    , node "link"
                        [ rel "stylesheet"
                        , type_ "text/css"
                        , href "style.css"
                        ]
                        []
                    , view model
                    ]
                }
        , subscriptions = subscriptions
        , onUrlChange = \_ -> Noop
        , onUrlRequest = \_ -> Noop
        }


init : ( FrontendModel, Cmd FrontendMsg )
init =
    ( { page = StartPage }
    , Cmd.none
    )


subscriptions : FrontendModel -> Sub FrontendMsg
subscriptions model =
    case model.page of
        GamePage gameModel ->
            Sub.map GameMsg (GamePage.subscriptions gameModel)

        _ ->
            Sub.none


devInit : ( FrontendModel, Cmd FrontendMsg )
devInit =
    case Character.create 5 of
        Just char ->
            updateFromBackend
                (LoggedIn
                    { username = ""
                    , loggedIn = Nothing
                    , passwordHash = Hash.fromString ""
                    , character = { char | coords = { x = 10 ^ 0, y = 0 } }
                    }
                    Dict.empty
                )
                { page = StartPage }

        Nothing ->
            ( { page = StartPage }
            , Cmd.none
            )


{-| This is the normal frontend update function. It handles all messages that can occur on the frontend.
-}
update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case ( msg, model.page ) of
        ( LoginMsg submsg, LoginPage submodel ) ->
            LoginPage.update submsg submodel
                |> with LoginMsg LoginPage model

        ( RegisterMsg submsg, RegisterPage submodel ) ->
            RegisterPage.update submsg submodel
                |> with RegisterMsg RegisterPage model

        ( GotTime account others time, _ ) ->
            GamePage.init account others time
                |> with GameMsg GamePage model

        ( GameMsg submsg, GamePage submodel ) ->
            GamePage.update submsg submodel |> with GameMsg GamePage model

        ( GotoLogin, _ ) ->
            ( { model | page = LoginPage LoginPage.init }, Cmd.none )

        ( GotoRegister, _ ) ->
            ( { model | page = RegisterPage RegisterPage.init }, Cmd.none )

        _ ->
            ( model, Cmd.none )


with msg page model =
    Tuple.mapBoth (\m -> { model | page = page m })
        (Cmd.map msg)


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case ( msg, model.page ) of
        ( LoggedIn account others, _ ) ->
            ( model, Task.perform (GotTime account others) Time.now )

        ( WrongUsernameOrPassword, LoginPage loginModel ) ->
            ( { page = LoginPage { loginModel | failed = True } }, Cmd.none )

        ( CheckNameResponse exists, RegisterPage registerModel ) ->
            if exists then
                ( { page = RegisterPage { registerModel | failed = True } }, Cmd.none )

            else
                ( { page = RegisterPage { registerModel | characterPicker = True } }, Cmd.none )

        ( UsernameAlreadyExists, RegisterPage registerModel ) ->
            ( { page = RegisterPage { registerModel | failed = True } }, Cmd.none )

        ( UpdateOtherPlayer username character, GamePage memory ) ->
            ( { model
                | page =
                    GamePage <|
                        { memory
                            | others =
                                memory.others
                                    |> Dict.update username
                                        (\prev ->
                                            Just ( character, Maybe.unwrap character.coords (Tuple.first >> .coords) prev )
                                        )
                        }
              }
            , Cmd.none
            )

        ( ChunkResponse x y chunk, GamePage game ) ->
            ( { model
                | page =
                    GamePage { game | chunks = Dict.insert ( x, y ) (Just chunk) game.chunks }
              }
            , Cmd.none
            )

        ( GotMessage message, GamePage game ) ->
            ( { model
                | page =
                    GamePage
                        { game
                            | messages = ( game.messageI, message ) :: game.messages |> List.take 10
                            , messageI = game.messageI + 1
                        }
              }
            , Process.sleep 30000 |> Task.perform (\_ -> GameMsg (RemoveMessage game.messageI))
            )

        ( OtherLoggedIn username, GamePage game ) ->
            ( { model
                | page =
                    GamePage
                        { game
                            | messages = ( game.messageI, SystemMessage (username ++ " logged in") ) :: game.messages |> List.take 10
                            , messageI = game.messageI + 1
                        }
              }
            , Process.sleep 30000 |> Task.perform (\_ -> GameMsg (RemoveMessage game.messageI))
            )

        _ ->
            ( model, Cmd.none )


view : FrontendModel -> Html FrontendMsg
view model =
    div [ class "main" ]
        [ case model.page of
            LoginPage loginmodel ->
                LoginPage.view loginmodel |> Html.map LoginMsg

            RegisterPage regmodel ->
                RegisterPage.view regmodel |> Html.map RegisterMsg

            GamePage gamemodel ->
                GamePage.view gamemodel |> Html.map GameMsg

            StartPage ->
                startView
        ]


startView : Html FrontendMsg
startView =
    div [ class "startPage" ]
        [ div [ class "form" ]
            [ Button.primary
                { action = Enabled GotoLogin
                , text = "Log in"
                , icon = Just Solid.signInAlt
                , attrs = []
                }
            , Button.primary
                { action = Enabled GotoRegister
                , text = "Register"
                , icon = Just Solid.userPlus
                , attrs = []
                }
            ]
        ]
