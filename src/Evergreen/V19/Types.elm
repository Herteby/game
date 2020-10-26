module Evergreen.V19.Types exposing (..)

import Evergreen.V19.Account
import AltMath.Vector2
import Browser.Dom
import Browser.Events
import Evergreen.V19.Character
import Evergreen.V19.Chunk
import Dict
import Evergreen.V19.Hash
import Keyboard.Key
import Evergreen.V19.Playground
import Evergreen.V19.Playground.Internal
import Time
import WebGL
import WebGL.Texture


type alias LoginModel = 
    { username : String
    , password : String
    , failed : Bool
    }


type alias RegisterModel = 
    { username : String
    , password : String
    , password2 : String
    , characterPicker : Bool
    , character : (Maybe Int)
    , failed : Bool
    , blurred : Bool
    }


type Message
    = SystemMessage String
    | UserMessage 
    { username : String
    , skin : Int
    , message : String
    }


type alias GameModel = 
    { time : Evergreen.V19.Playground.Time
    , screen : Evergreen.V19.Playground.Screen
    , visibility : Browser.Events.Visibility
    , keyboard : (List Keyboard.Key.Key)
    , textures : Evergreen.V19.Playground.Internal.TextureManager
    , entities : (List WebGL.Entity)
    , player : Evergreen.V19.Character.Character
    , others : (Dict.Dict String (Evergreen.V19.Character.Character, AltMath.Vector2.Vec2))
    , chunks : (Dict.Dict (Int, Int) (Evergreen.V19.Chunk.Request Evergreen.V19.Chunk.Chunk))
    , messages : (List (Int, Message))
    , chatInput : (Maybe String)
    , messageI : Int
    , showPlayerList : Bool
    , lastUpdate : (Int, Evergreen.V19.Character.Character)
    , fps : (List Int)
    , showMinimap : Bool
    , starting : Bool
    , initialLoad : (Maybe Int)
    }


type Page
    = StartPage
    | LoginPage LoginModel
    | RegisterPage RegisterModel
    | GamePage GameModel


type alias FrontendModel =
    { page : Page
    }


type alias BackendModel =
    { accounts : (List Evergreen.V19.Account.Account)
    , chunks : (Dict.Dict (Int, Int) Evergreen.V19.Chunk.Chunk)
    }


type LoginMsg
    = LoginUsername String
    | LoginPassword String
    | Submit


type RegisterMsg
    = InputUsername String
    | InputPassword String
    | InputPassword2 String
    | Blurred
    | Next
    | SelectedCharacter Int
    | Register


type GameMsg
    = KeyDown Keyboard.Key.Key
    | KeyUp Keyboard.Key.Key
    | Tick Time.Posix
    | GotViewport Browser.Dom.Viewport
    | Resized Evergreen.V19.Playground.Screen
    | VisibilityChanged Browser.Events.Visibility
    | GotTexture (Result (String, WebGL.Texture.Error) (String, WebGL.Texture.Texture))
    | ChatInput String
    | ToggleMinimap
    | TogglePlayerList
    | RemoveMessage Int
    | Noop2


type FrontendMsg
    = LoginMsg LoginMsg
    | RegisterMsg RegisterMsg
    | GameMsg GameMsg
    | GotoLogin
    | GotoRegister
    | GotTime Evergreen.V19.Account.Account (Dict.Dict String Evergreen.V19.Character.Character) Time.Posix
    | Noop


type ToBackend
    = CheckName String
    | CreateAccount String Evergreen.V19.Hash.Hash Int
    | Login String Evergreen.V19.Hash.Hash
    | UpdatePlayer Evergreen.V19.Character.Character
    | SendMessage String
    | GetChunk Int Int


type BackendMsg
    = BNoop


type ToFrontend
    = LoggedIn Evergreen.V19.Account.Account (Dict.Dict String Evergreen.V19.Character.Character)
    | OtherLoggedIn String
    | CheckNameResponse Bool
    | WrongUsernameOrPassword
    | UsernameAlreadyExists
    | UpdateOtherPlayer String Evergreen.V19.Character.Character
    | GotMessage Message
    | ChunkResponse Int Int Evergreen.V19.Chunk.Chunk