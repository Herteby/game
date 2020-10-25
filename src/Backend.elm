module Backend exposing (app)

import Character exposing (Character)
import Dict
import Env
import Lamdera exposing (ClientId, SessionId)
import List.Extra as List
import Types exposing (..)
import World


app =
    Lamdera.backend
        { init = init
        , update = update
        , subscriptions = \m -> Sub.none
        , updateFromFrontend = updateFromFrontend
        }


init : ( BackendModel, Cmd BackendMsg )
init =
    ( { accounts = []
      , chunks = Dict.empty
      }
    , Cmd.none
    )


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update msg model =
    case msg of
        BNoop ->
            ( model, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        CreateAccount username passwordHash skin ->
            if List.any (\a -> a.username == username) model.accounts then
                ( model, Lamdera.sendToFrontend clientId UsernameAlreadyExists )

            else
                case Character.create skin of
                    Just character ->
                        let
                            account =
                                { username = username
                                , passwordHash = passwordHash
                                , character = character
                                , loggedIn = Just clientId
                                }

                            others =
                                model.accounts
                                    |> List.filterMap
                                        (\a ->
                                            if a.loggedIn == Nothing then
                                                Nothing

                                            else
                                                Just ( a.username, a.character )
                                        )
                                    |> Dict.fromList

                            notifyOthers =
                                model.accounts
                                    |> List.filterMap .loggedIn
                                    |> List.map
                                        (\id ->
                                            Lamdera.sendToFrontend id (OtherLoggedIn account.username)
                                        )
                        in
                        ( { model
                            | accounts = account :: model.accounts
                          }
                        , Cmd.batch <|
                            Lamdera.sendToFrontend clientId (LoggedIn account others)
                                :: notifyOthers
                        )

                    Nothing ->
                        ( model, Cmd.none )

        Login username passwordHash ->
            let
                match a =
                    a.username == username && a.passwordHash == passwordHash
            in
            case List.find match model.accounts of
                Just account ->
                    let
                        account_ =
                            { account | loggedIn = Just clientId }

                        others =
                            model.accounts
                                |> List.filterMap
                                    (\a ->
                                        if a.loggedIn == Nothing || a.username == account.username then
                                            Nothing

                                        else
                                            Just ( a.username, a.character )
                                    )
                                |> Dict.fromList

                        notifyOthers =
                            model.accounts
                                |> List.filterMap .loggedIn
                                |> List.filter (\id -> id /= clientId)
                                |> List.map
                                    (\id ->
                                        Lamdera.sendToFrontend id (OtherLoggedIn account.username)
                                    )
                    in
                    ( { model | accounts = List.setIf match account_ model.accounts }
                    , Cmd.batch <|
                        Lamdera.sendToFrontend clientId (LoggedIn account_ others)
                            :: notifyOthers
                    )

                Nothing ->
                    ( model, Lamdera.sendToFrontend clientId WrongUsernameOrPassword )

        UpdatePlayer character ->
            case List.find (\a -> a.loggedIn == Just clientId) model.accounts of
                Just account ->
                    ( { model
                        | accounts =
                            List.setIf (\a -> a.loggedIn == Just clientId)
                                { account | character = character }
                                model.accounts
                      }
                    , model.accounts
                        |> List.filterMap .loggedIn
                        |> List.filter (\id -> id /= clientId)
                        |> List.map (\id -> Lamdera.sendToFrontend id (UpdateOtherPlayer account.username character))
                        |> Cmd.batch
                    )

                Nothing ->
                    ( model, Cmd.none )

        CheckName username ->
            let
                exists =
                    List.any (\a -> a.username == username) model.accounts
            in
            ( model, Lamdera.sendToFrontend clientId (CheckNameResponse exists) )

        GetChunk x y ->
            case Dict.get ( x, y ) model.chunks of
                Just chunk ->
                    ( model, Lamdera.sendToFrontend clientId (ChunkResponse x y chunk) )

                Nothing ->
                    let
                        chunk =
                            World.generateChunk x y
                    in
                    ( case Env.mode of
                        Env.Development ->
                            model

                        Env.Production ->
                            { model | chunks = Dict.insert ( x, y ) chunk model.chunks }
                    , Lamdera.sendToFrontend clientId (ChunkResponse x y chunk)
                    )

        SendMessage message ->
            ( model
            , case List.find (\a -> a.loggedIn == Just clientId) model.accounts of
                Just sender ->
                    model.accounts
                        |> List.filterMap .loggedIn
                        |> List.map
                            (\id ->
                                Lamdera.sendToFrontend id <|
                                    GotMessage <|
                                        UserMessage
                                            { username = sender.username
                                            , skin = sender.character.skin
                                            , message = message
                                            }
                            )
                        |> Cmd.batch

                Nothing ->
                    Cmd.none
            )
