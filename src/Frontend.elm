module Frontend exposing (app, init)

import AltMath.Vector2 exposing (Vec2)
import Browser.Dom
import Browser.Events
import Character
import Dict exposing (Dict)
import Env
import FontAwesome.Icon exposing (Icon)
import FontAwesome.Solid as Solid
import FontAwesome.Styles
import GamePage
import Hash
import Html exposing (..)
import Html.Attributes as Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Keyed
import Html.Lazy exposing (..)
import Json.Decode as Decode
import Lamdera
import LoginPage
import Maybe.Extra as Maybe
import Playground exposing (Time)
import Playground.Advanced as Playground
import Process
import RegisterPage
import Set
import Task
import Time
import Types exposing (..)
import UI exposing (none, px, textSpan)
import UI.Button as Button exposing (Action(..))
import UI.Icon as Icon


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


subscriptions : FrontendModel -> Sub FrontendMsg
subscriptions model =
    Sub.batch
        [ Sub.map GameMsg Playground.subscriptions.all
        , Browser.Events.onKeyDown (Decode.field "code" Decode.string |> Decode.map KeyDown)
        ]


init : ( FrontendModel, Cmd FrontendMsg )
init =
    ( { page = StartPage }
    , Cmd.none
    )


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

        ( GameMsg submsg, GamePage submodel ) ->
            GamePage.game.update submsg submodel
                |> (\( newmodel, cmd ) ->
                        let
                            ( computer, newMemory ) =
                                Playground.get newmodel

                            ( cx, cy ) =
                                newMemory.player.coords
                                    |> (\{ x, y } ->
                                            ( round <| x / (64 * 16) - 0.5, round <| y / (64 * 16) - 0.5 )
                                       )

                            ( lastUpdate, cmd_ ) =
                                if newMemory.player /= Tuple.second newMemory.lastUpdate && (Playground.now computer.time > Time.posixToMillis (Tuple.first newMemory.lastUpdate) + 250) then
                                    ( ( Time.millisToPosix <| Playground.now computer.time, newMemory.player )
                                    , Lamdera.sendToBackend (UpdatePlayer newMemory.player)
                                    )

                                else
                                    ( newMemory.lastUpdate, Cmd.none )

                            newNewModel =
                                newmodel |> Playground.edit (\_ mem -> { mem | lastUpdate = lastUpdate })
                        in
                        ( { page = GamePage newNewModel }
                        , Cmd.batch
                            [ Cmd.map GameMsg cmd
                            , cmd_
                            ]
                        )
                            |> checkChunk cx cy
                            |> checkChunk (cx + 1) cy
                            |> checkChunk cx (cy + 1)
                            |> checkChunk (cx + 1) (cy + 1)
                   )

        ( KeyDown code, GamePage submodel ) ->
            if code == "Enter" then
                let
                    ( _, memory ) =
                        Playground.get submodel
                in
                case memory.chatInput of
                    Just message ->
                        ( { model | page = GamePage (submodel |> Playground.edit (\_ mem -> { mem | chatInput = Nothing })) }
                        , if message == "" then
                            Cmd.none

                          else
                            Lamdera.sendToBackend (SendMessage message)
                        )

                    Nothing ->
                        ( { model | page = GamePage (submodel |> Playground.edit (\_ mem -> { mem | chatInput = Just "" })) }
                        , Browser.Dom.focus "chatInput" |> Task.attempt (always Noop)
                        )

            else
                ( model, Cmd.none )

        ( ChatInput message, GamePage submodel ) ->
            ( { model | page = GamePage (submodel |> Playground.edit (\_ mem -> { mem | chatInput = Just message })) }, Cmd.none )

        ( TogglePlayerList, GamePage submodel ) ->
            ( { model | page = GamePage (submodel |> Playground.edit (\_ mem -> { mem | showPlayerList = not mem.showPlayerList })) }, Cmd.none )

        ( RemoveMessage i, GamePage submodel ) ->
            ( { model
                | page =
                    GamePage
                        (submodel
                            |> Playground.edit (\_ mem -> { mem | messages = List.filter (\( i_, m ) -> i_ /= i) mem.messages })
                        )
              }
            , Cmd.none
            )

        ( GotoLogin, _ ) ->
            ( { model | page = LoginPage LoginPage.init }, Cmd.none )

        ( GotoRegister, _ ) ->
            ( { model | page = RegisterPage RegisterPage.init }, Cmd.none )

        _ ->
            ( model, Cmd.none )


checkChunk x y ( model, cmd ) =
    case model.page of
        GamePage game ->
            let
                memory =
                    Playground.get game |> Tuple.second
            in
            if Dict.get ( x, y ) memory.chunks == Nothing then
                ( { page = GamePage (game |> Playground.edit (\_ _ -> { memory | chunks = Dict.insert ( x, y ) Pending memory.chunks })) }
                , Cmd.batch [ cmd, Lamdera.sendToBackend (GetChunk x y) ]
                )

            else
                ( model, cmd )

        _ ->
            ( model, cmd )


with msg page model =
    Tuple.mapBoth (\m -> { model | page = page m })
        (Cmd.map msg)


{-| This is the added update function. It handles all messages that can arrive from the backend.
-}
updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case ( msg, model.page ) of
        ( LoggedIn account others, _ ) ->
            GamePage.init account others
                |> with GameMsg GamePage model

        ( WrongUsernameOrPassword, LoginPage loginModel ) ->
            ( { page = LoginPage { loginModel | failed = True } }, Cmd.none )

        ( CheckNameResponse exists, RegisterPage registerModel ) ->
            if exists then
                ( { page = RegisterPage { registerModel | failed = True } }, Cmd.none )

            else
                ( { page = RegisterPage { registerModel | characterPicker = True } }, Cmd.none )

        ( UsernameAlreadyExists, RegisterPage registerModel ) ->
            ( { page = RegisterPage { registerModel | failed = True } }, Cmd.none )

        ( UpdateOtherPlayer username character, GamePage game ) ->
            ( { model
                | page =
                    GamePage <|
                        Playground.edit
                            (\_ memory ->
                                { memory
                                    | others =
                                        memory.others
                                            |> Dict.update username
                                                (\prev ->
                                                    Just ( character, Maybe.unwrap character.coords (Tuple.first >> .coords) prev )
                                                )
                                }
                            )
                            game
              }
            , Cmd.none
            )

        ( ChunkResponse x y chunk, GamePage game ) ->
            ( { model
                | page =
                    GamePage <|
                        Playground.edit
                            (\_ memory ->
                                { memory | chunks = Dict.insert ( x, y ) (Received chunk) memory.chunks }
                            )
                            game
              }
            , Cmd.none
            )

        ( GotMessage message, GamePage game ) ->
            let
                ( _, mem ) =
                    Playground.get game
            in
            ( { model
                | page =
                    GamePage <|
                        Playground.edit
                            (\_ memory ->
                                { memory
                                    | messages = ( memory.messageI, message ) :: memory.messages |> List.take 10
                                    , messageI = memory.messageI + 1
                                }
                            )
                            game
              }
            , Process.sleep 30000 |> Task.perform (\_ -> RemoveMessage mem.messageI)
            )

        ( OtherLoggedIn username, GamePage game ) ->
            let
                ( _, mem ) =
                    Playground.get game
            in
            ( { model
                | page =
                    GamePage <|
                        Playground.edit
                            (\_ memory ->
                                { memory
                                    | messages = ( memory.messageI, SystemMessage (username ++ " logged in") ) :: memory.messages |> List.take 10
                                    , messageI = memory.messageI + 1
                                }
                            )
                            game
              }
            , Process.sleep 30000 |> Task.perform (\_ -> RemoveMessage mem.messageI)
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
                let
                    ( computer, memory ) =
                        Playground.get gamemodel
                in
                div []
                    [ GamePage.game.view gamemodel
                    , lazy2 chat memory.messages memory.chatInput
                    , info memory
                    , namePlates memory.player memory.others
                    , if memory.showPlayerList then
                        playerList memory.others

                      else
                        none
                    ]

            StartPage ->
                startView
        ]


info : Memory -> Html msg
info { fps, player } =
    div [ class "coords" ]
        [ div [] [ text <| "fps: " ++ (String.fromInt <| List.sum fps // List.length fps) ]
        , div [] [ text ("x: " ++ String.fromInt (round player.coords.x)) ]
        , div [] [ text ("y: " ++ String.fromInt (round player.coords.y)) ]
        ]


chat : List ( Int, Message ) -> Maybe String -> Html FrontendMsg
chat messages chatInput =
    div [ class "bottomLeft" ]
        [ div [ class "chat" ]
            [ messages
                |> List.reverse
                |> List.map
                    (\( i, m ) ->
                        ( String.fromInt i
                        , case m of
                            UserMessage { username, skin, message } ->
                                div [ class "message" ]
                                    [ div [ class "avatar", style "background-image" ("url(" ++ Character.url skin ++ ")") ] []
                                    , div []
                                        [ div [ class "username" ] [ text username ]
                                        , div [] [ text message ]
                                        ]
                                    ]

                            SystemMessage message ->
                                div [ class "system message" ]
                                    [ text message
                                    ]
                        )
                    )
                |> Html.Keyed.node "div" []
            , case chatInput of
                Just message ->
                    input
                        [ class "chatInput"
                        , value message
                        , onInput ChatInput
                        , id "chatInput"
                        ]
                        []

                Nothing ->
                    div [ class "chatHint" ] [ text "Press enter to chat" ]
            ]
        , uiButton Solid.users "Open player list" TogglePlayerList
        ]


uiButton : Icon -> String -> msg -> Html msg
uiButton icon title msg =
    button
        [ class "uiButton"
        , onClick msg
        , Attributes.title title
        ]
        [ Icon.view [] icon ]


namePlates : Character -> Dict String ( Character, Vec2 ) -> Html FrontendMsg
namePlates player others =
    div [ class "namePlates overlay" ]
        [ Dict.toList others
            |> List.map
                (\( username, ( _, { x, y } ) ) ->
                    div
                        [ class "namePlate"
                        , style "top" (px ((player.coords.y - y) * 2))
                        , style "left" (px ((x - player.coords.x) * 2))
                        ]
                        [ text username ]
                )
            |> div []
        ]


playerList : Dict String ( Character, Vec2 ) -> Html FrontendMsg
playerList players =
    div [ class "overlay", onClick TogglePlayerList ]
        [ div [ class "modal" ]
            [ div [ class "header" ] [ textSpan "Players" ]
            , div [ class "body" ]
                (Dict.toList players
                    |> List.map
                        (\( username, ( character, coords ) ) ->
                            div [ class "player" ]
                                [ div [ class "avatar", style "background-image" ("url(" ++ Character.url character.skin ++ ")") ] []
                                , textSpan username
                                , textSpan ("x: " ++ String.fromInt (round character.coords.x))
                                , textSpan ("y: " ++ String.fromInt (round character.coords.y))
                                ]
                        )
                )
            ]
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
