module Types exposing (..)

import Lamdera exposing (ClientId)
import Set exposing (Set)
import Keyboard
import Playground
import Playground.Advanced as Playground
import Playground.Extra as Playground

type alias FrontendModel =
    { messages : List ChatMsg, messageFieldContent : String
    , game : Playground.Game Memory
    }

type alias Memory = {
    character:Character
    }

type alias Character = {
    coords:(Float,Float)
    }


type alias BackendModel =
    { messages : List Message, clients : Set ClientId }


type FrontendMsg
    = MessageFieldChanged String
    | MessageSubmitted
    | GameMsg Playground.Msg
    | Noop


type ToBackend
    = ClientJoin
    | MsgSubmitted String


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
