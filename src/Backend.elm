module Backend exposing (Model, app)

import Lamdera exposing (ClientId, SessionId)
import Set exposing (Set, map)
import Task
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
    ( { messages = [], clients = Set.empty }, Cmd.none )


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
                    { model | clients = Set.insert clientId model.clients }

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


broadcast clients msg =
    clients
        |> Set.toList
        |> List.map (\clientId -> Lamdera.sendToFrontend clientId msg)
        |> Cmd.batch
