module Frontend exposing (app, init)

import Browser.Dom
import Browser.Events
import Character
import Css
import Dict
import GamePage
import Hash
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Keyed
import Html.Lazy exposing (..)
import Json.Decode as Decode
import Lamdera
import LoginPage
import Misc exposing (none)
import Playground.Advanced as Playground
import Process
import RegisterPage
import Task
import Types exposing (..)


{-| Lamdera applications define 'app' instead of 'main'.

Lamdera.frontend is the same as Browser.application with the
additional update function; updateFromBackend.

-}
app =
    Lamdera.frontend
        { init =
            \_ _ ->
                if devMode then
                    devInit

                else
                    init
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
                    , character = char
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
                            newMemory =
                                Playground.get newmodel |> Tuple.second

                            oldMemory =
                                Playground.get submodel |> Tuple.second

                            ( cx, cy ) =
                                newMemory.player.coords
                                    |> (\( x, y ) ->
                                            ( round <| x / (64 * 16), round <| y / (64 * 16) )
                                       )

                            cmd_ =
                                if newMemory.player /= oldMemory.player then
                                    Lamdera.sendToBackend (UpdatePlayer newMemory.player)

                                else
                                    Cmd.none
                        in
                        ( { page = GamePage newmodel }
                        , Cmd.batch
                            [ Cmd.map GameMsg cmd
                            , cmd_
                            ]
                        )
                            |> checkChunk cx cy
                            |> checkChunk (cx - 1) cy
                            |> checkChunk cx (cy - 1)
                            |> checkChunk (cx - 1) (cy - 1)
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
                                { memory | others = Dict.insert username character memory.others }
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

        _ ->
            ( model, Cmd.none )


view : FrontendModel -> Html FrontendMsg
view model =
    div [ class "main" ]
        [ Html.node "style" [] [ text Css.css ]
        , case model.page of
            LoginPage loginmodel ->
                LoginPage.view loginmodel |> Html.map LoginMsg

            RegisterPage regmodel ->
                RegisterPage.view regmodel |> Html.map RegisterMsg

            GamePage gamemodel ->
                let
                    ( _, memory ) =
                        Playground.get gamemodel
                in
                div []
                    [ GamePage.game.view gamemodel
                    , lazy2 chat memory.messages memory.chatInput
                    , viewCoords memory.player.coords
                    ]

            StartPage ->
                startView
        ]


viewCoords coords =
    div [ class "coords" ]
        [ div [] [ text ("x: " ++ (Tuple.first >> round >> String.fromInt) coords) ]
        , div [] [ text ("y: " ++ (Tuple.second >> round >> String.fromInt) coords) ]
        ]


chat messages chatInput =
    div [ class "chat" ]
        [ messages
            |> List.reverse
            |> List.map
                (\( i, m ) ->
                    ( String.fromInt i
                    , div [ class "message" ]
                        [ div [ class "avatar", style "background-image" ("url(" ++ Character.url m.skin ++ ")") ] []
                        , div []
                            [ div [ class "username" ] [ text m.username ]
                            , div [] [ text m.message ]
                            ]
                        ]
                    )
                )
            |> Html.Keyed.node "div" []
        , case chatInput of
            Just message ->
                input [ value message, onInput ChatInput, id "chatInput" ] []

            Nothing ->
                div [ class "chatHint" ] [ text "Press enter to chat" ]
        ]


startView =
    div [ class "startPage" ]
        [ button [ onClick GotoLogin, class "big" ] [ text "Log in" ]
        , button [ onClick GotoRegister, class "big" ] [ text "Register" ]
        ]
