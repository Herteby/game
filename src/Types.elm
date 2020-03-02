module Types exposing (..)

import Character exposing (Character)
import Lamdera exposing (ClientId)
import Playground
import Set exposing (Set)


type alias FrontendModel =
    { messages : List ChatMsg
    , messageFieldContent : String
    , game : Playground.Game Memory
    }


type alias Memory =
    { character : Character
    }


type alias BackendModel =
    { messages : List Message
    , clients : Set ClientId
    , accounts : List Account
    }


type alias Account =
    { username : String
    , passwordHash : String
    }


type FrontendMsg
    = MessageFieldChanged String
    | MessageSubmitted
    | GameMsg Playground.Msg
    | SelectedCharacter Int
    | Noop


type ToBackend
    = ClientJoin
    | MsgSubmitted String
    | CreateAccount Account
    | Login Account


type BackendMsg
    = BNoop


type ToFrontend
    = ClientJoinReceived ClientId
    | ClientTimeoutReceived ClientId
    | RoomMsgReceived Message


type alias Message =
    ( String, String )


type ChatMsg
    = ClientJoined ClientId
    | ClientTimedOut ClientId
    | MsgReceived ClientId String
