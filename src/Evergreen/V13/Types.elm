module Evergreen.V13.Types exposing (..)

import AltMath.Vector2
import Dict
import Lamdera
import Matrix
import Evergreen.V13.Playground
import Time


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


type Direction
    = Up
    | Down
    | Left
    | Right


type Speed
    = Standing
    | Walking
    | Sprinting


type alias Character = 
    { coords : AltMath.Vector2.Vec2
    , direction : Direction
    , speed : Speed
    , skin : Int
    }


type Terrain
    = Water
    | Beach
    | Dirt
    | DirtDark
    | Grass
    | GrassDark
    | GrassDry
    | Snow
    | Volcanic
    | Magma
    | Pond


type TreeColor
    = Green
    | Yellow
    | Orange


type TreeVariant
    = T1
    | T2
    | T3
    | T4


type ConiferSnow
    = NoSnow
    | WithSnow


type ConiferVariant
    = C1
    | C2
    | C3
    | C4
    | C5


type DeadTreeVariant
    = D1
    | D2
    | D3
    | D4
    | D5


type FlowerColor
    = FlowerRed
    | FlowerYellow
    | FlowerBlue
    | FlowerPurple
    | FlowerPink


type FlowerVariant
    = F1
    | F2
    | F3
    | F4


type Object
    = Tree TreeColor TreeVariant
    | Conifer ConiferSnow ConiferVariant
    | DeadTree DeadTreeVariant
    | Shell Int
    | Flower FlowerColor FlowerVariant


type alias Chunk = 
    { textures : (List (Terrain, String))
    , terrain : (Matrix.Matrix Terrain)
    , objects : (List (List (Maybe Object)))
    }


type Request a
    = Pending
    | Received a


type Message
    = SystemMessage String
    | UserMessage 
    { username : String
    , skin : Int
    , message : String
    }


type alias Memory = 
    { player : Character
    , others : (Dict.Dict String (Character, AltMath.Vector2.Vec2))
    , chunks : (Dict.Dict (Int, Int) (Request Chunk))
    , messages : (List (Int, Message))
    , chatInput : (Maybe String)
    , messageI : Int
    , showPlayerList : Bool
    , lastUpdate : (Time.Posix, Character)
    , fps : (List Int)
    }


type Page
    = StartPage
    | LoginPage LoginModel
    | RegisterPage RegisterModel
    | GamePage (Evergreen.V13.Playground.Game Memory)


type alias FrontendModel =
    { page : Page
    }


type Hash
    = Hash String


type alias Account = 
    { username : String
    , passwordHash : Hash
    , character : Character
    , loggedIn : (Maybe Lamdera.ClientId)
    }


type alias BackendModel =
    { accounts : (List Account)
    , chunks : (Dict.Dict (Int, Int) Chunk)
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


type FrontendMsg
    = LoginMsg LoginMsg
    | RegisterMsg RegisterMsg
    | GameMsg Evergreen.V13.Playground.Msg
    | GotoLogin
    | GotoRegister
    | KeyDown String
    | ChatInput String
    | ChatSubmit
    | TogglePlayerList
    | RemoveMessage Int
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
    = LoggedIn Account (Dict.Dict String Character)
    | OtherLoggedIn String
    | CheckNameResponse Bool
    | WrongUsernameOrPassword
    | UsernameAlreadyExists
    | UpdateOtherPlayer String Character
    | GotMessage Message
    | ChunkResponse Int Int Chunk