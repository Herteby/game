module Evergreen.V9.Types exposing (..)

import AltMath.Vector2 exposing (Vec2)
import Dict exposing (Dict)
import Lamdera exposing (ClientId)
import Matrix exposing (Matrix)
import Playground


type Hash
    = Hash String


type alias Character =
    { coords : Vec2
    , direction : Direction
    , speed : Speed
    , skin : Int
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


type alias Chunk =
    { textures : List ( Terrain, String )
    , terrain : Matrix Terrain
    , objects : List (List (Maybe Object))
    }


type Object
    = Tree TreeColor TreeVariant
    | Conifer ConiferSnow ConiferVariant
    | DeadTree DeadTreeVariant
    | Shell Int
    | Flower Int


type TreeVariant
    = T1
    | T2
    | T3
    | T4


type TreeColor
    = Green
    | Yellow
    | Orange


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


type ConiferSnow
    = NoSnow
    | WithSnow


devMode =
    False


type alias FrontendModel =
    { page : Page
    }


type Page
    = StartPage
    | LoginPage LoginModel
    | RegisterPage RegisterModel
    | GamePage (Playground.Game Memory)


type alias Memory =
    { player : Character
    , others : Dict String Character
    , chunks : Dict ( Int, Int ) (Request Chunk)
    , messages : List ( Int, Message )
    , chatInput : Maybe String
    , messageI : Int
    }


type alias Message =
    { username : String, skin : Int, message : String }


type Request a
    = Pending
    | Received a


type alias BackendModel =
    { accounts : List Account
    , chunks : Dict ( Int, Int ) Chunk
    }


type alias Account =
    { username : String
    , passwordHash : Hash
    , character : Character
    , loggedIn : Maybe ClientId
    }


type FrontendMsg
    = LoginMsg LoginMsg
    | RegisterMsg RegisterMsg
    | GameMsg Playground.Msg
    | GotoLogin
    | GotoRegister
    | KeyDown String
    | ChatInput String
    | ChatSubmit
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
    = LoggedIn Account (Dict String Character)
    | OtherLoggedIn Int String
    | CheckNameResponse Bool
    | WrongUsernameOrPassword
    | UsernameAlreadyExists
    | UpdateOtherPlayer String Character
    | GotMessage Message
    | ChunkResponse Int Int Chunk


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


type alias LoginModel =
    { username : String
    , password : String
    , failed : Bool
    }


type LoginMsg
    = LoginUsername String
    | LoginPassword String
    | Submit
