module Types exposing (..)

import Lamdera exposing (ClientId)
import Set exposing (Set)
import Keyboard
import Game.Resources  exposing (Resources)

type alias FrontendModel =
    { messages : List ChatMsg, messageFieldContent : String
    , keys : List Keyboard.Key
    ,time : Float
    , screen : (Int,Int)
    , resources : Resources
    , character : Character
    }

type alias Character = {
    coords:(Float,Float)
    }


type alias BackendModel =
    { messages : List Message, clients : Set ClientId }


type FrontendMsg
    = MessageFieldChanged String
    | MessageSubmitted
    | ScreenSize Int Int
    | Tick Float
    | Keys Keyboard.Msg
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
