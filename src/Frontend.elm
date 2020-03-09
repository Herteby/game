module Frontend exposing (Model, app)

import Character
import Css
import Dict
import GamePage
import Hash
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
    if True then
        devInit

    else
        ( { page = StartPage }
        , Cmd.none
        )


devInit : ( Model, Cmd FrontendMsg )
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
updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
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
        [ button [ onClick GotoLogin, class "big" ] [ text "Log in" ]
        , button [ onClick GotoRegister, class "big" ] [ text "Register" ]
        ]
