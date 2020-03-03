module Backend exposing (Model, app)

import Dict
import Lamdera exposing (ClientId, SessionId)
import Types exposing (..)


app =
    Lamdera.backend
        { init = init
        , update = update
        , subscriptions = \m -> Sub.none
        , updateFromFrontend = updateFromFrontend
        }


type alias Model =
    BackendModel


init : ( Model, Cmd BackendMsg )
init =
    ( { messages = []
      , clients = Dict.empty
      , accounts = []
      }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        BNoop ->
            ( model, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        -- A new client has joined! Add them to our clients list, and send them all messages we have so far.
        ClientJoin ->
            let
                newModel =
                    { model | clients = Dict.insert clientId NotLoggedIn model.clients }

                sendHelloMessageToAllClients =
                    broadcast newModel.clients (ClientJoinReceived clientId)

                sendMessageHistoryToNewlyJoinedClient =
                    model.messages
                        -- |> List.reverse -- Que? Is this a bug?
                        |> List.map RoomMsgReceived
                        |> List.map (Lamdera.sendToFrontend clientId)
                        |> Cmd.batch
            in
            ( newModel
            , Cmd.batch
                [ sendHelloMessageToAllClients
                , sendMessageHistoryToNewlyJoinedClient
                ]
            )

        -- A client has sent us a new message! Add it to our messages list, and broadcast it to everyone.
        MsgSubmitted text ->
            ( { model | messages = ( clientId, text ) :: model.messages }
            , broadcast model.clients (RoomMsgReceived ( clientId, text ))
            )

        CreateAccount account ->
            if List.any (\a -> a.username == account.username) model.accounts then
                ( model, Cmd.none )

            else
                ( { model | accounts = account :: model.accounts }, Cmd.none )

        Login account ->
            if List.any (\a -> a.username == account.username && a.passwordHash == account.passwordHash) model.accounts then
                ( model, Cmd.none )

            else
                ( model, Cmd.none )


broadcast clients msg =
    clients
        |> Dict.keys
        |> List.map (\clientId -> Lamdera.sendToFrontend clientId msg)
        |> Cmd.batch
