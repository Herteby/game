module Types exposing (..)

import Account exposing (Account)
import AltMath.Vector2 exposing (Vec2)
import Character exposing (Character)
import Chunk exposing (Chunk)
import Dict exposing (Dict)
import Hash exposing (Hash)
import Playground exposing (Game)
import Time


type alias FrontendModel =
    { page : Page
    }


type Page
    = StartPage
    | LoginPage LoginModel
    | RegisterPage RegisterModel
    | GamePage (Game Memory)


type alias BackendModel =
    { accounts : List Account
    , chunks : Dict ( Int, Int ) Chunk
    }


type FrontendMsg
    = LoginMsg LoginMsg
    | RegisterMsg RegisterMsg
    | GameMsg GameMsg
    | GotoLogin
    | GotoRegister
    | Noop


type ToBackend
    = CheckName String
    | CreateAccount String Hash Int
    | Login String Hash
    | UpdatePlayer Character
    | SendMessage String
    | GetChunk Int Int


type BackendMsg
    = BNoop


type ToFrontend
    = LoggedIn Account (Dict String Character)
    | OtherLoggedIn String
    | CheckNameResponse Bool
    | WrongUsernameOrPassword
    | UsernameAlreadyExists
    | UpdateOtherPlayer String Character
    | GotMessage Message
    | ChunkResponse Int Int Chunk


type alias Memory =
    { player : Character
    , others : Dict String ( Character, Vec2 )
    , chunks : Dict ( Int, Int ) (Maybe Chunk)
    , messages : List ( Int, Message )
    , chatInput : Maybe String
    , messageI : Int
    , showPlayerList : Bool
    , lastUpdate : ( Time.Posix, Character )
    , fps : List Int
    , showMinimap : Bool
    }


type GameMsg
    = PlaygroundMsg Playground.Msg
    | KeyDown String
    | ChatInput String
    | ToggleMinimap
    | TogglePlayerList
    | RemoveMessage Int
    | Noop2


type Message
    = SystemMessage String
    | UserMessage { username : String, skin : Int, message : String }


type alias LoginModel =
    { username : String
    , password : String
    , failed : Bool
    }


type LoginMsg
    = LoginUsername String
    | LoginPassword String
    | Submit


type alias RegisterModel =
    { username : String
    , password : String
    , password2 : String
    , characterPicker : Bool
    , character : Maybe Int
    , failed : Bool
    , blurred : Bool
    }


type RegisterMsg
    = InputUsername String
    | InputPassword String
    | InputPassword2 String
    | Blurred
    | Next
    | SelectedCharacter Int
    | Register
