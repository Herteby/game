module Types exposing (..)

import Account exposing (Account)
import AltMath.Vector2 exposing (Vec2)
import Browser.Dom
import Browser.Events exposing (Visibility)
import Character exposing (Character)
import Chunk exposing (Chunk)
import Dict exposing (Dict)
import Hash exposing (Hash)
import Keyboard.Key exposing (Key)
import Playground exposing (Keyboard, Screen, Time)
import Playground.Internal exposing (TextureManager)
import Time
import WebGL exposing (Entity)
import WebGL.Texture as Texture exposing (Texture)


type alias FrontendModel =
    { page : Page
    }


type Page
    = StartPage
    | LoginPage LoginModel
    | RegisterPage RegisterModel
    | GamePage GameModel


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
    | GotTime Account (Dict String Character) Time.Posix
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


type alias GameModel =
    { time : Time
    , screen : Screen
    , visibility : Visibility
    , keyboard : List Key
    , textures : TextureManager
    , entities : List Entity
    , player : Character
    , others : Dict String ( Character, Vec2 )
    , chunks : Dict ( Int, Int ) (Maybe Chunk)
    , messages : List ( Int, Message )
    , chatInput : Maybe String
    , messageI : Int
    , showPlayerList : Bool
    , lastUpdate : ( Int, Character )
    , fps : List Int
    , showMinimap : Bool
    }


type GameMsg
    = KeyDown Key
    | KeyUp Key
    | Tick Time.Posix
    | GotViewport Browser.Dom.Viewport
    | Resized Screen
    | VisibilityChanged Browser.Events.Visibility
    | GotTexture (Result ( String, Texture.Error ) ( String, Texture ))
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
